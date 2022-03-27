-module(otel_interceptor_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

all() ->
    [trace_interceptor].

init_per_suite(Config) ->
    application:load(grpcbox),
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(trace_interceptor, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{exporter => undefined}}]),
    application:set_env(grpcbox, client,
                        #{channels => [{default_channel,
                                        [{http, "localhost", 8080, []}],
                                        #{unary_interceptor => fun otel_grpcbox_interceptor:unary_client/7}}]}),
    application:ensure_all_started(opentelemetry),
    application:set_env(grpcbox, servers,
                        [#{grpc_opts =>
                               #{service_protos => [route_guide_pb],
                                 services => #{'routeguide.RouteGuide' => routeguide_route_guide},
                                 unary_interceptor =>
                                     grpcbox_chain_interceptor:unary([fun otel_grpcbox_interceptor:unary/4,
                                                                      fun ?MODULE:trace_to_trailer/4])}}]),
    application:ensure_all_started(grpcbox),
    Config.

%% include a trace context and test that it works by having a second interceptor add
%% the trace id from the context as a response trailer.
trace_interceptor(_Config) ->
    Point = #{latitude => 409146138, longitude => -746188906},
    ?with_span(<<"grpc-client-call">>, #{},
               fun(_) ->
                       {_, _Feature, #{trailers := Trailers}} =
                           routeguide_route_guide_client:get_feature(Point),
                       SpanCtx = ?current_span_ctx,
                       BinTraceId = integer_to_binary(otel_span:trace_id(SpanCtx)),
                       ?assertEqual(BinTraceId, maps:get(<<"x-grpc-trace-id">>, Trailers))

               end),
    ok.

%%

trace_to_trailer(Ctx, Message, _ServerInfo, Handler) ->
    SpanCtx = ?current_span_ctx,
    BinTraceId = integer_to_binary(otel_span:trace_id(SpanCtx)),
    Trailer = grpcbox_metadata:pairs([{<<"x-grpc-trace-id">>, BinTraceId}]),
    Ctx1 = grpcbox_stream:add_trailers(Ctx, Trailer),
    Handler(Ctx1, Message).
