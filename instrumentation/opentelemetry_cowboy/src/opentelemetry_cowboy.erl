-module(opentelemetry_cowboy).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-export([
         setup/0,
         setup/1,
         handle_event/4,
         default_public_endpoint_fn/2]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/client_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/error_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/network_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/server_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/user_agent_attributes.hrl").

-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/http_attributes.hrl").

-define(TRACER_ID, ?MODULE).

?MODULEDOC("""
OpenTelemetry instrumentation for `cowboy`.

## Semantic Conventions

All required and recommended Server HTTP Span semantic conventions are implemented.
Supported opt-in attributes can be configured using the `opt_in_attrs` option.

## Options

### Opt-in Semantic Convention Attributes

Otel SemConv requires users to explicitly opt in for any attribute with a
requirement level of `opt-in`. To ensure compatibility, always use the
SemConv attribute.

Example:
```
OptInAttrs = [{?HTTP_REQUEST_BODY_SIZE, true}]`
opentelemetry_cowboy:setup(#{opt_in_attrs => OptInAttrs)
```

#### Request and Response Headers as Opt-in Attributes

Request and response header attributes are opt-in and can be set with the
`request_headers` and `response_headers` options. Values should be lower-case.

```
opentelemetry_cowboy:setup(#{request_headers => ["x-customer-id"]})
```

### Public Endpoint

Setting an endpoint as public will result in any propagated trace to be added as a link,
rather than a continuation of an existing trace. The `public_endpoint` option should be set
to `true` if an endpoint only accepts public traffic to prevent missing root spans. By default,
the endpoint is handled as non-public, resulting in traces being continued rather than linked.

In a mixed traffic environment, an MFA can be supplied to determine whether to
treat a request as public. This function is executed on every request, so refrain
from expensive operations such as lookups to external systems. The function must
be a predicate function of arity-2, accepting the `Req` from the request as
the first argument and user-supplied options as the second. Any dynamic
information used in comparisons should be supplied at setup time for efficiency.

Example:
```
-module(public_endpoint).

is_public_request(Req, Opts) ->
    # return true if request was public

opentelemetry_cowboy:setup(#{public_endpoint_fn => {public_endpoint, fun is_public_request/2, []}}).
```
""").

default_opts() ->
    #{
        client_address_headers => [<<"forwarded">>, <<"x-forwarded-for">>],
        client_headers_sort_fn => undefined,
        handler_id => otel_cowboy,
        opt_in_attrs => [],
        public_endpoint => false,
        public_endpoint_fn => {?MODULE, default_public_endpoint_fn, []},
        request_headers => [],
        response_headers => [],
        scheme_headers => [<<"forwarded">>, <<"x-forwarded-proto">>],
        scheme_headers_sort_fn => undefined,
        server_address_headers => [<<"forwarded">>, <<"x-forwarded-host">>, <<"host">>],
        server_address_headers_sort_fn => undefined
    }.

?DOC("""
Initializes and configures the telemetry handlers.

Supported options:
* `client_address_headers` - Headers to use for extracting original client request address info. Default: `[<<"forwarded">>, <<"x-forwarded-for">>]`
* `client_headers_sort_fn` - Custom client header sort fn. See `otel_http` for more info. Default: `undefined`
* `handler_id` - Only set when running multiple instances on different endpoints. Default: `otel_cowboy`
* `opt_in_attrs` - Use semantic conventions library to ensure compatibility, e.g. `[{?HTTP_REQUEST_BODY_SIZE, true}]`. Default: `[]`
* `public_endpoint` - Endpoint is public. Propagated traces will be added as a link. Default: `false`
* `public_endpoint_fn` - Default function returns `false`. See docs for more info.
* `request_headers` - List of request headers to add as attributes. (lowercase). Default: `[]`
* `response_headers` - List of response headers to add as attributes. (lowercase). Default: `[]`
* `scheme_headers` - Headers to use for extracting original client request scheme. Default: `[<<"forwarded">>, <<"x-forwarded-proto">>]`
* `scheme_headers_sort_fn` - Custom scheme header sort fn. See `otel_http` for more info. Default: `undefined`
* `server_address_headers` - Headers to use for extracting original server address info. Default: `[<<"forwarded">>, <<"x-forwarded-host">>, <<"host">>]`
* `server_address_headers_sort_fn` - Custom server header sort fn. See `otel_http` for more info. Default: `undefined`
""").
-spec setup() -> ok.
setup() ->
    setup(default_opts()).

-spec setup([] | map()) -> ok.
setup(Opts) when is_list(Opts) ->
    setup(maps:from_list(Opts));
setup(Opts) ->
    InitialConfig = maps:merge(default_opts(), Opts),
    OptInAttrs = maps:get(opt_in_attrs, InitialConfig),
    ReversedClientAddressHeaders = lists:reverse(maps:get(client_address_headers, InitialConfig)),
    ReversedSchemeHeaders = lists:reverse(maps:get(scheme_headers, InitialConfig)),
    FinalOpts = maps:merge(InitialConfig, #{
        client_address_headers => ReversedClientAddressHeaders,
        opt_in_attrs => OptInAttrs,
        scheme_headers => ReversedSchemeHeaders
    }),
    attach_event_handlers(FinalOpts),
    ok.

attach_event_handlers(Config) ->
    Events = [
              [cowboy, request, early_error],
              [cowboy, request, start],
              [cowboy, request, stop],
              [cowboy, request, exception]
             ],
    telemetry:attach_many({?MODULE, maps:get(handler_id, Config)}, Events, fun ?MODULE:handle_event/4, Config).

parse_method(Method) ->
    case Method of
        <<"CONNECT">> -> ?HTTP_REQUEST_METHOD_VALUES_CONNECT;
        <<"DELETE">> -> ?HTTP_REQUEST_METHOD_VALUES_DELETE;
        <<"GET">> -> ?HTTP_REQUEST_METHOD_VALUES_GET;
        <<"HEAD">> -> ?HTTP_REQUEST_METHOD_VALUES_HEAD;
        <<"OPTIONS">> -> ?HTTP_REQUEST_METHOD_VALUES_OPTIONS;
        <<"PATCH">> -> ?HTTP_REQUEST_METHOD_VALUES_PATCH;
        <<"POST">> -> ?HTTP_REQUEST_METHOD_VALUES_POST;
        <<"PUT">> -> ?HTTP_REQUEST_METHOD_VALUES_PUT;
        <<"TRACE">> -> ?HTTP_REQUEST_METHOD_VALUES_TRACE;
        _ -> ?HTTP_REQUEST_METHOD_VALUES_OTHER
    end.

extract_headers(Headers, Keys) ->
    maps:filter(fun(K,_V) -> lists:member(K, Keys) end, Headers).

set_req_header_attrs(Attrs, _ReqHeaders, #{request_headers := []}) -> Attrs;
set_req_header_attrs(Attrs, ReqHeaders, #{request_headers := HeadersAttrs}) ->
    maps:merge(Attrs, otel_http:extract_headers_attributes(request, ReqHeaders, HeadersAttrs)).

set_resp_header_attrs(Attrs, _RespHeaders, #{response_headers := []}) -> Attrs;
set_resp_header_attrs(Attrs, RespHeaders, #{response_headers := HeadersAttrs}) ->
    maps:merge(Attrs, otel_http:extract_headers_attributes(response, RespHeaders, HeadersAttrs)).

otel_http_extract_client_info(Headers, undefined) ->
    otel_http:extract_client_info(Headers);
otel_http_extract_client_info(Headers, SortFn) ->
    % sort expects a list - need to convert
    otel_http:extract_client_info(maps:to_list(Headers), SortFn).

extract_client_address(Req, Config) ->
    #{
        client_address_headers := ClientAddrHeaders,
        client_headers_sort_fn := SortFn
    } = Config,
    #{
        headers := Headers,
        peer := {PeerRemoteIP, PeerPort}
    } = Req,
    ClientHeaders = extract_headers(Headers, ClientAddrHeaders),
    case otel_http_extract_client_info(ClientHeaders, SortFn) of
        #{ip := undefined} ->
            #{ip => ip_to_binary(PeerRemoteIP), port => PeerPort};
        ClientAddress ->
            ClientAddress
    end.


extract_server_address(ReqHeaders, Config) ->
    #{
        server_address_headers := ServerAddrHeaders,
        server_address_headers_sort_fn := SortFn
    } = Config,
    ServerHeaders = extract_headers(ReqHeaders, ServerAddrHeaders),
    otel_http_extract_server_info(ServerHeaders, SortFn).

otel_http_extract_server_info(Headers, undefined) ->
    otel_http:extract_server_info(Headers);
otel_http_extract_server_info(Headers, SortFn) ->
    otel_http:extract_server_info(maps:to_list(Headers), SortFn).

set_server_address_attrs(Attrs, ReqHeaders, Config) ->
    case extract_server_address(ReqHeaders, Config) of
        #{address := undefined} ->
            Attrs;
        #{address := Address, port := undefined} ->
            maps:put(?SERVER_ADDRESS, Address, Attrs);
        #{address := Address, port := Port} ->
            maps:merge(Attrs, #{
                ?SERVER_ADDRESS => Address,
                ?SERVER_PORT => Port
            })
    end.

otel_http_extract_scheme(Headers, undefined) ->
    otel_http:extract_scheme(Headers);
otel_http_extract_scheme(Headers, SortFn) ->
    otel_http:extract_scheme(maps:to_list(Headers), SortFn).

extract_scheme(Req, Config) ->
    #{
        scheme_headers := SchemeHeaders,
        scheme_headers_sort_fn := SortFn
    } = Config,
    #{
        headers := Headers,
        scheme := ReqScheme
    } = Req,
    SchemeHeaders1 = extract_headers(Headers, SchemeHeaders),
    case otel_http_extract_scheme(SchemeHeaders1, SortFn) of
        undefined ->
            case ReqScheme of
                <<"http">> ->
                    http;
                <<"https">> ->
                    https
            end;
        ParsedScheme ->
            ParsedScheme
    end.


ip_to_binary(IP) ->
    iolist_to_binary(inet:ntoa(IP)).

is_public_endpoint(_Req, #{public_endpoint := true}) -> true;
is_public_endpoint(Req, #{public_endpoint_fn := {M, F, A}}) ->
    apply(M, F, [Req, A]).

default_public_endpoint_fn(_, _) -> false.

opt_in_attrs() ->
    #{
        ?CLIENT_PORT => undefined,
        ?NETWORK_LOCAL_ADDRESS => undefined,
        ?NETWORK_LOCAL_PORT => undefined,
        ?NETWORK_TRANSPORT => ?NETWORK_TRANSPORT_VALUES_TCP
    }.

handle_event([cowboy, request, start], _Measurements, #{req := Req} = Meta, Config) ->
    #{
        headers := ReqHeaders,
        host := LocalHost,
        method := ReqMethod,
        path := Path,
        peer := {PeerRemoteIP, PeerPort},
        port := LocalPort,
        qs := QueryString,
        version := Version
    } = Req,
    #{opt_in_attrs := OptedInAttrs} = Config,
    #{ip := ClientIP, port := ClientPort} = extract_client_address(Req, Config),
    DefaultOptInAttrs = opt_in_attrs(),
    OptInAttrs = DefaultOptInAttrs#{
        ?CLIENT_PORT := ClientPort,
        ?NETWORK_LOCAL_ADDRESS := LocalHost,
        ?NETWORK_LOCAL_PORT := LocalPort
    },

    Method = parse_method(ReqMethod),

    Attrs1 = #{
                ?CLIENT_ADDRESS => ClientIP,
                ?HTTP_REQUEST_METHOD => Method,
                ?NETWORK_PEER_ADDRESS => ip_to_binary(PeerRemoteIP),
                ?NETWORK_PEER_PORT => PeerPort,
                ?URL_PATH => Path,
                ?URL_SCHEME => extract_scheme(Req, Config),
                ?USER_AGENT_ORIGINAL => maps:get(<<"user-agent">>, ReqHeaders, <<"">>)},
    Attrs2 = set_network_protocol_attrs(Attrs1, Version),
    Attrs3 = set_query_string_attr(Attrs2, QueryString),
    Attrs4 = set_server_address_attrs(Attrs3, ReqHeaders, Config),
    Attrs5 = set_req_header_attrs(Attrs4, ReqHeaders, Config),
    AttrsFinal = maps:merge(Attrs5, maps:filter(fun(K,_V) -> lists:member(K, OptedInAttrs) end, OptInAttrs)),

    SpanName =
        case Method of
            ?HTTP_REQUEST_METHOD_VALUES_OTHER ->
                'HTTP';
            _ ->
                Method
            end,
    case is_public_endpoint(Req, Config) of
        false ->
            otel_propagator_text_map:extract(maps:to_list(ReqHeaders)),
            otel_telemetry:start_telemetry_span(?TRACER_ID, SpanName, Meta, #{
                attributes => AttrsFinal,
                kind => ?SPAN_KIND_SERVER
            });
        true ->
            PropagatedCtx = otel_propagator_text_map:extract_to(otel_ctx:new(), maps:to_list(ReqHeaders)),
            SpanCtx = otel_tracer:current_span_ctx(PropagatedCtx),
            otel_telemetry:start_telemetry_span(?TRACER_ID, SpanName, Meta, #{
                attributes => AttrsFinal,
                kind => ?SPAN_KIND_SERVER,
                links => opentelemetry:links([SpanCtx])
            })
    end;


handle_event([cowboy, request, stop], Measurements, Meta, Config) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    #{
        resp_headers := RespHeaders,
        resp_status := Status
    } = Meta,
    #{
        opt_in_attrs := OptedInAttrs
    } = Config,
    #{
        req_body_length := ReqBodyLength,
        resp_body_length := RespBodyLength
    } = Measurements,
    % these are opt-in
    OptInAttrs = #{
                  ?HTTP_REQUEST_BODY_SIZE => ReqBodyLength,
                  ?HTTP_RESPONSE_BODY_SIZE => RespBodyLength
                 },
    StatusCode = transform_status_to_code(Status),

    case StatusCode of
        undefined ->
            case maps:get(error, Meta, undefined) of
              {ErrorType, Error, Reason} ->
                otel_span:add_events(Ctx, [opentelemetry:event(ErrorType, #{error => Error, reason => Reason})]),
                otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, Reason));
              _ ->
                % do nothing first as I'm unsure how should we handle this
                ok
            end;
        StatusCode when StatusCode >= 500 ->
            Attrs = set_resp_header_attrs(#{
                    ?HTTP_RESPONSE_STATUS_CODE => StatusCode,
                    ?ERROR_TYPE => integer_to_binary(StatusCode)
                }, RespHeaders, Config),
            FinalAttrs =
                maps:merge(Attrs, maps:filter(fun(K,_V) -> lists:member(K, OptedInAttrs) end, OptInAttrs)),
            otel_span:set_attributes(Ctx, FinalAttrs),

            otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"">>));
        _ ->
            Attrs = set_resp_header_attrs(#{
                    ?HTTP_RESPONSE_STATUS_CODE => StatusCode
                }, RespHeaders, Config),
            FinalAttrs =
                maps:merge(Attrs, maps:filter(fun(K,_V) -> lists:member(K, OptedInAttrs) end, OptInAttrs)),
            otel_span:set_attributes(Ctx, FinalAttrs),
            ok
    end,
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear();

handle_event([cowboy, request, exception], Measurements, Meta, Config) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    #{
        opt_in_attrs := OptedInAttrs
    } = Config,
    #{
        req_body_length := ReqBodyLength,
        resp_body_length := RespBodyLength
    } = Measurements,
    #{
      kind := Kind,
      reason := Reason,
      stacktrace := Stacktrace,
      resp_headers := RespHeaders,
      resp_status := Status
     } = Meta,

    % these are opt-in
    OptInAttrs = #{
                  ?HTTP_REQUEST_BODY_SIZE => ReqBodyLength,
                  ?HTTP_RESPONSE_BODY_SIZE => RespBodyLength
                 },
    StatusCode = transform_status_to_code(Status),
    ErrorType =
        case Reason of
            R when is_atom(R) ->
                otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, []),
                R;
            {{#{message := ElixirExceptionMessage, '__struct__' := ElixirException, '__exception__' := true}, ElixirStacktrace}, _} ->
                otel_span:record_exception(Ctx, Kind, ElixirException, ElixirExceptionMessage, ElixirStacktrace, []),
                ElixirException;
            _ ->
                Reason
        end,
    Attrs = set_resp_header_attrs(#{
            ?HTTP_RESPONSE_STATUS_CODE => StatusCode,
            ?ERROR_TYPE => ErrorType
        }, RespHeaders, Config),
    FinalAttrs =
        maps:merge(Attrs, maps:filter(fun(K,_V) -> lists:member(K, OptedInAttrs) end, OptInAttrs)),

    otel_span:set_attributes(Ctx, FinalAttrs),

    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"">>)),

    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear();

handle_event([cowboy, request, early_error], Measurements, Meta, Config) ->
    #{
        opt_in_attrs := OptedInAttrs
    } = Config,
    #{
        resp_body_length := RespBodyLength
    } = Measurements,
    #{
      reason := {ErrorType, Error, Reason},
      resp_headers := RespHeaders,
      resp_status := Status
     } = Meta,

    OptInAttrs = #{?HTTP_RESPONSE_BODY_SIZE => RespBodyLength},

    StatusCode = transform_status_to_code(Status),

    Attrs = set_resp_header_attrs(#{
            ?HTTP_RESPONSE_STATUS_CODE => StatusCode,
            ?ERROR_TYPE => integer_to_binary(StatusCode)
        }, RespHeaders, Config),
    FinalAttrs =
        maps:merge(Attrs, maps:filter(fun(K,_V) -> lists:member(K, OptedInAttrs) end, OptInAttrs)),

    Ctx = otel_telemetry:start_telemetry_span(?TRACER_ID, 'HTTP', Meta, #{attributes => FinalAttrs, kind => ?SPAN_KIND_SERVER}),
    otel_span:add_events(Ctx, [opentelemetry:event(ErrorType, #{error => Error, reason => Reason})]),
    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, Reason)),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta),
    otel_ctx:clear().

transform_status_to_code(Status) when is_binary(Status) ->
  [CodeString | _Message] = string:split(Status, " "),
  {Code, _Rest} = string:to_integer(CodeString),
  Code;
transform_status_to_code(Status) ->
  Status.

set_query_string_attr(Attrs, <<"">>) -> Attrs;
set_query_string_attr(Attrs, QueryString) ->
    maps:put(?URL_QUERY, QueryString, Attrs).

set_network_protocol_attrs(Attrs, ReqVersion) ->
    case extract_network_protocol(ReqVersion) of
        {error, _Reason} ->
            Attrs;

        {http, Version} ->
            maps:merge(Attrs, #{?NETWORK_PROTOCOL_VERSION => Version});

        {Protocol, Version} ->
            maps:merge(Attrs, #{
                ?NETWORK_PROTOCOL_NAME => Protocol,
                ?NETWORK_PROTOCOL_VERSION => Version
            })
    end.

extract_network_protocol(Version) ->
    case Version of
        'HTTP/1.0' -> {http, '1.0'};
        'HTTP/1.1' -> {http, '1.1'};
        'HTTP/2.0' -> {http, '2'};
        'HTTP/2' -> {http, '2'};
        'SPDY' -> {'SPDY', '2'};
        'QUIC' -> {'QUIC', '3'};
        _ -> {error, <<"Invalid protocol">>}
    end.
