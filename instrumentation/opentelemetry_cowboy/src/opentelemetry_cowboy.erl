-module(opentelemetry_cowboy).

-export([
         setup/0,
         setup/1,
         handle_event/4]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-define(TRACER_ID, ?MODULE).

-spec setup() -> ok.
setup() ->
    setup([]).

-spec setup([]) -> ok.
setup(_Opts) ->
    attach_event_handlers(),
    ok.

attach_event_handlers() ->
    Events = [
              [cowboy, request, early_error],
              [cowboy, request, start],
              [cowboy, request, stop],
              [cowboy, request, exception]
             ],
    telemetry:attach_many(opentelemetry_cowboy_handlers, Events, fun ?MODULE:handle_event/4, #{}).

handle_event([cowboy, request, start], _Measurements, #{req := Req} = Meta, _Config) ->
    Headers = maps:get(headers, Req),
    otel_propagator_text_map:extract(maps:to_list(Headers)),
    {RemoteIP, _Port} = maps:get(peer, Req),
    Method = maps:get(method, Req),

    Attributes = #{
                  'http.client_ip' => client_ip(Headers, RemoteIP),
                  'http.flavor' => http_flavor(Req),
                  'http.host' => maps:get(host, Req),
                  'http.host.port' => maps:get(port, Req),
                  'http.method' => Method,
                  'http.scheme' => maps:get(scheme, Req),
                  'http.target' => maps:get(path, Req),
                  'http.user_agent' => maps:get(<<"user-agent">>, Headers, <<"">>),
                  'net.host.ip' => iolist_to_binary(inet:ntoa(RemoteIP)),
                  'net.transport' => 'IP.TCP'
                 },
    SpanName = iolist_to_binary([<<"HTTP ">>, Method]),
    otel_telemetry:start_telemetry_span(?TRACER_ID, SpanName, Meta, #{attributes => Attributes});

handle_event([cowboy, request, stop], Measurements, Meta, _Config) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    Status = maps:get(resp_status, Meta),
    Attributes = #{
                  'http.request_content_length' => maps:get(req_body_length, Measurements),
                  'http.response_content_length' => maps:get(resp_body_length, Measurements)
                 },
    otel_span:set_attributes(Ctx, Attributes),
    case Status of
        undefined ->
            {ErrorType, Error, Reason} = maps:get(error, Meta),
            otel_span:add_events(Ctx, [opentelemetry:event(ErrorType, #{error => Error, reason => Reason})]),
            otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, Reason));
        Status when Status >= 400 ->
            otel_span:set_attribute(Ctx, 'http.status', Status),
            otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"">>));
        Status when Status < 400 ->
            otel_span:set_attribute(Ctx, 'http.status', Status)
    end,
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear();

handle_event([cowboy, request, exception], Measurements, Meta, _Config) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    #{
      kind := Kind,
      reason := Reason,
      stacktrace := Stacktrace,
      resp_status := Status
     } = Meta,
    otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, []),
    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"">>)),
    otel_span:set_attributes(Ctx, #{
                                   'http.status' => Status,
                                   'http.request_content_length' => maps:get(req_body_length, Measurements),
                                   'http.response_content_length' => maps:get(resp_body_length, Measurements)
                                  }),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear();

handle_event([cowboy, request, early_error], Measurements, Meta, _Config) ->
    #{
      reason := {ErrorType, Error, Reason},
      resp_status := Status
     } = Meta,
    Attributes = #{
                   'http.status' => Status,
                   'http.response_content_length' => maps:get(resp_body_length, Measurements)
                  },
    Ctx = otel_telemetry:start_telemetry_span(?TRACER_ID, <<"HTTP Error">>, Meta, #{attributes => Attributes}),
    otel_span:add_events(Ctx, [opentelemetry:event(ErrorType, #{error => Error, reason => Reason})]),
    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, Reason)),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear().

http_flavor(Req) ->
    case maps:get(version, Req, undefined) of
        'HTTP/1.0' -> '1.0';
        'HTTP/1.1' -> '1.1';
        'HTTP/2' -> '2.0';
        'SPDY' -> 'SPDY';
        'QUIC' -> 'QUIC';
        _ -> <<"">>
    end.

client_ip(Headers, RemoteIP) ->
  case maps:get(<<"x-forwarded-for">>, Headers, undefined) of
      undefined ->
          iolist_to_binary(inet:ntoa(RemoteIP));
      Addresses ->
          hd(binary:split(Addresses, <<",">>))
  end.
