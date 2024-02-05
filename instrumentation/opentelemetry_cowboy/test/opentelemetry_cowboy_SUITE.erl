-module(opentelemetry_cowboy_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

all() ->
    [
     successful_request,
     chunked_request,
     failed_request,
     client_timeout_request,
     idle_timeout_request,
     chunk_timeout_request,
     bad_request,
     binary_status_code_request
     ].

init_per_suite(Config) ->
    ok = application:load(opentelemetry),
    {ok,_} = application:ensure_all_started(ranch),
    Dispatch = cowboy_router:compile([{"localhost", [
                                      {"/success", test_h, success},
                                      {"/chunked", test_h, chunked},
                                      {"/chunked_slow", test_h, chunked_slow},
                                      {"/slow", test_h, slow},
                                      {"/failure", test_h, failure},
                                      {"/binary_status_code", test_h,
                                       binary_status_code}
                                     ]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
                  env => #{dispatch => Dispatch},
                  stream_handlers => [cowboy_telemetry_h, cowboy_stream_h],
                  idle_timeout => 150
              }
    ),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    application:stop(ranch),
    application:stop(telemetry).

init_per_testcase(_, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),

    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, _} = application:ensure_all_started(opentelemetry_telemetry),
    {ok, _} = application:ensure_all_started(opentelemetry_cowboy),
    opentelemetry_cowboy:setup(),

    otel_batch_processor:set_exporter(otel_exporter_pid, self()),

    Config.

end_per_testcase(_, Config) ->
    Config.

successful_request(_Config) ->
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
               {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success", Headers}, [], []),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"203.0.133.195">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"GET">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/success">>,
                             'http.user_agent' => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.status_code' => 200,
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 12},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(successful_request)
    end.

chunked_request(_Config) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/chunked", []}, [], []),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"127.0.0.1">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"GET">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/chunked">>,
                             'http.user_agent' => <<>>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.status_code' => 200,
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 14},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(chunked_request)
    end.

failed_request(_Config) ->
    {ok, {{_Version, 500, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/failure", []}, [], []),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name=exception} = Event,
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"127.0.0.1">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"GET">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/failure">>,
                             'http.user_agent' => <<>>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.status_code' => 500,
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 0},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(failed_request)
    end.

client_timeout_request(_Config) ->
    {error, timeout} =
        httpc:request(get, {"http://localhost:8080/slow", []}, [{timeout, 50}], []),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name='socket_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => closed,
                                   reason => 'The socket has been closed.'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"127.0.0.1">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"GET">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/slow">>,
                             'http.user_agent' => <<>>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 0},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(client_timeout_request)
    end.

idle_timeout_request(_Config) ->
    {error, socket_closed_remotely} =
        httpc:request(head, {"http://localhost:8080/slow", []}, [], []),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name= 'connection_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => timeout,
                                   reason => 'Connection idle longer than configuration allows.'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual(<<"HTTP HEAD">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"127.0.0.1">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"HEAD">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/slow">>,
                             'http.user_agent' => <<>>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 0},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(idle_timeout_request)
    end.

chunk_timeout_request(_Config) ->
    httpc:request(head, {"http://localhost:8080/chunked_slow", []}, [], []),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            ?assertEqual(<<"HTTP HEAD">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"127.0.0.1">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"HEAD">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/chunked_slow">>,
                             'http.user_agent' => <<>>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.status_code' => 200,
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 0},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(chunk_timeout_request)
    end.

bad_request(_Config) ->
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"}],
    {ok, {{_Version, 501, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(trace, {"http://localhost:8080/", Headers}, [], []),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name='connection_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => no_error,
                                   reason => 'The TRACE method is currently not implemented. (RFC7231 4.3.8)'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual(<<"HTTP Error">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             'http.status_code' => 501,
                             'http.response_content_length' => 0},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(bad_request)
    end.

binary_status_code_request(_Config) ->
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
               {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/binary_status_code", Headers}, [], []),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ExpectedAttrs = #{
                             'http.client_ip' => <<"203.0.133.195">>,
                             'http.flavor' => '1.1',
                             'http.host' => <<"localhost">>,
                             'http.host.port' => 8080,
                             'http.method' => <<"GET">>,
                             'http.scheme' => <<"http">>,
                             'http.target' => <<"/binary_status_code">>,
                             'http.user_agent' => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                             'net.host.ip' => <<"127.0.0.1">>,
                             'net.transport' => 'IP.TCP',
                             'http.status_code' => 200,
                             'http.request_content_length' => 0,
                             'http.response_content_length' => 12
                            },
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(binary_status_code_request)
    end.
