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
    {ok, _} = application:ensure_all_started(telemetry_registry),
    {ok, _} = application:ensure_all_started(test_app),
    {ok, _} = application:ensure_all_started(opentelemetry_telemetry),
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    otel_telemetry:trace_application(test_app),
    opentelemetry:register_tracer(test_tracer, "0.1.0"),
    Config.

end_per_testcase(_, Config) ->
    application:stop(telemetry),
    application:stop(telemetry_registry),
    application:stop(test_app),
    application:stop(opentelemetry_telemetry),
    application:stop(opentelemetry),
    Config.

telemetry_span_handling(_Config) ->
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx1),
    _Result = test_app:handler(ok),
    ?assertMatch(SpanCtx1, ?current_span_ctx),
    try test_app:handler(raise_exception) of
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
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentId,span_id=Id}} ->
            {Id, ParentId}
    after
        5000 ->
            error(timeout)
    end.

exception_span_listener(Name) ->
    receive
        {span, #span{name=Name,events=Events,status=Status,parent_span_id=ParentId,span_id=Id}} ->
            ?assertEqual({status,error,<<"badarg">>}, Status),
            ?assertEqual(1, erlang:length(Events)),
            {Id, ParentId}
    after
        5000 ->
            error(timeout)
    end.
