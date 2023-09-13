-module(otel_telemetry_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

all() -> [
          telemetry_span_handling
         ].

init_per_suite(Config) ->
    ok = application:load(opentelemetry_telemetry),
    ok = application:load(opentelemetry),
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    Config.

end_per_suite(_Config) ->
    ok = application:unload(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(opentelemetry_telemetry),
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    attach_event_handlers(),
    Config.

end_per_testcase(_, Config) ->
    application:stop(telemetry),
    application:stop(opentelemetry_telemetry),
    application:stop(opentelemetry),
    Config.

telemetry_span_handling(_Config) ->
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx1),
    _Result = handler(ok),
    ?assertMatch(SpanCtx1, ?current_span_ctx),
    try handler(raise_exception) of
        _ -> ok
    catch
        error:badarg -> ok
    end,
    ?assertMatch(SpanCtx1, ?current_span_ctx),
    ?set_attribute(<<"attribute">>, 1),
    ?end_span(),
    {_, Span3Parent} = successful_span_listener(<<"test_app_nested_span">>),
    {Span2, Span2Parent} = successful_span_listener(<<"test_app_handler">>),
    {_, ExceptionSpanParent} = exception_span_listener(<<"test_app_handler">>),
    {Span1, undefined} = successful_span_listener(<<"span-1">>),
    ?assertEqual(Span2Parent, Span1),
    ?assertEqual(ExceptionSpanParent, Span1),
    ?assertEqual(Span3Parent, Span2),
    ok.

successful_span_listener(Name) ->
    receive
        {span, #span{name=Name,parent_span_id=ParentId,span_id=Id}} ->
            {Id, ParentId}
    after
        5000 ->
            error(timeout)
    end.

exception_span_listener(Name) ->
    receive
        {span, #span{name=Name,events=Events,status={status,error,Reason},parent_span_id=ParentId,span_id=Id} = Span} ->
            ?assertEqual(<<"badarg">>, Reason),
            ?assertEqual(1, erlang:length(otel_events:list(Events))),
            {Id, ParentId}
    after
        5000 ->
            error(timeout)
    end.

handler(Args) ->
    _ = telemetry:span(
          [test_app, handler],
          #{},
          fun() ->
                  case Args of
                      raise_exception ->
                          binary_to_list("heh, already a list");
                      _ -> {nested_span(), #{}}
                  end
          end).

nested_span() ->
    _ = telemetry:span(
          [test_app, nested_span],
          #{},
          fun() ->
              {ok, #{}}
          end).

-define(TRACER_ID, ?MODULE).

attach_event_handlers() ->
    Events = [
              [test_app, handler, start],
              [test_app, handler, stop],
              [test_app, handler, exception],
              [test_app, nested_span, start],
              [test_app, nested_span, stop],
              [test_app, nested_span, exception]
             ],
    telemetry:attach_many(otel_telemetry_test_handlers, Events, fun ?TRACER_ID:handle_event/4, #{}).

handle_event([test_app, handler, start], _, Meta, _) ->
    otel_telemetry:start_telemetry_span(?TRACER_ID, <<"test_app_handler">>, Meta, #{});

handle_event([test_app, handler, stop], _, Meta, _) ->
    otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta);

handle_event([test_app, handler, exception], _, Meta, _) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    #{
      kind := Kind,
      reason := Reason,
      stacktrace := Stacktrace
     } = Meta,
    otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, []),
    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, atom_to_binary(Reason))),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta);

handle_event([test_app, nested_span, start], _, Meta, _) ->
    otel_telemetry:start_telemetry_span(?TRACER_ID, <<"test_app_nested_span">>, Meta, #{});

handle_event([test_app, nested_span, stop], _, Meta, _) ->
    otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta);

handle_event([test_app, nested_span, exception], _, Meta, _) ->
    Ctx = otel_telemetry:set_current_telemetry_span(?TRACER_ID, Meta),
    #{
      kind := Kind,
      reason := Reason,
      stacktrace := Stacktrace
     } = Meta,
    otel_span:set_status(Ctx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"">>)),
    otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, []),
    otel_telemetry:end_telemetry_span(?TRACER_ID, Meta).
