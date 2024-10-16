-module(opentelemetry_cowboy_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-include_lib("opentelemetry_semantic_conventions/include/attributes/client_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/error_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/network_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/server_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/user_agent_attributes.hrl").

-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/http_attributes.hrl").

all() ->
    [
        successful_request_with_default_config,
        public_endpoint_fun,
        public_endpoint_true,
        with_all_optins,
        successful_request_with_custom_header_setting,
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

    otel_batch_processor:set_exporter(otel_exporter_pid, self()),

    Config.

end_per_testcase(_, Config) ->
    telemetry:detach({opentelemetry_cowboy, otel_cowboy}),
    Config.

successful_request_with_default_config(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62650,
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"forwarded", "host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=https;by=203.0.113.43"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
               {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success?a=b", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual('GET', Name),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"192.0.2.60">>,
                             ?HTTP_REQUEST_METHOD => 'GET',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"developer.mozilla.org">>,
                             ?SERVER_PORT => 4321,
                             ?URL_PATH => <<"/success">>,
                             ?URL_QUERY => <<"a=b">>,
                             ?URL_SCHEME => https,
                             ?USER_AGENT_ORIGINAL => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                             ?HTTP_RESPONSE_STATUS_CODE => 200},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(successful_request)
    end.

public_endpoint_true(_Config) ->
    OptIns = #{
        public_endpoint => true
    },
    opentelemetry_cowboy:setup(OptIns),
    Port = 62658,
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success?a=b", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{parent_span_id=ParentSpanId}} ->
            ?assertEqual(undefined, ParentSpanId)
    after
        1000 -> ct:fail(successful_request)
    end.

public_endpoint_fun(_Config) ->
    OptIns = #{
        public_endpoint_fn => {?MODULE, public_endpoint_true_fn, []}
    },
    opentelemetry_cowboy:setup(OptIns),
    os:putenv("TENANT", "customer"),
    Port = 62659,
    Headers = [
                {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
                {"tracestate", "congo=t61rcWkgMzE"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success?a=b", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{parent_span_id=ParentSpanId}} ->
            ?assertEqual(undefined, ParentSpanId)
    after
        1000 -> ct:fail(successful_request)
    end,
    os:unsetenv("TENANT").

public_endpoint_true_fn(_Req, _Opts) ->
    os:getenv("TENANT") /= "internal".

with_all_optins(_Config) ->
    Opts = #{
      opt_in_attrs => [
        ?CLIENT_PORT,
        ?HTTP_REQUEST_BODY_SIZE,
        ?HTTP_RESPONSE_BODY_SIZE,
        ?NETWORK_LOCAL_ADDRESS,
        ?NETWORK_LOCAL_PORT,
        ?NETWORK_TRANSPORT
      ],
      request_headers => [<<"test-header">>],
      response_headers => [<<"content-type">>]
    },
    opentelemetry_cowboy:setup(Opts),
    Port = 62660,
    Headers = [
                {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
                {"tracestate", "congo=t61rcWkgMzE"},
                {"test-header", "request header"},
                {"forwarded", "host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=https;by=203.0.113.43"},
                {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
                {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success?a=b", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual('GET', Name),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                                ?CLIENT_ADDRESS => <<"192.0.2.60">>,
                                ?CLIENT_PORT => undefined,
                                ?HTTP_REQUEST_BODY_SIZE => 0,
                                ?HTTP_REQUEST_METHOD => 'GET',
                                ?HTTP_RESPONSE_BODY_SIZE => 12,
                                ?NETWORK_LOCAL_ADDRESS => <<"localhost">>,
                                ?NETWORK_LOCAL_PORT => 8080,
                                ?NETWORK_TRANSPORT => tcp,
                                'http.request.header.test-header' => [<<"request header">>],
                                'http.response.header.content-type' => [<<"text/plain">>],
                                ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                                ?NETWORK_PEER_PORT => Port,
                                ?NETWORK_PROTOCOL_VERSION => '1.1',
                                ?SERVER_ADDRESS => <<"developer.mozilla.org">>,
                                ?SERVER_PORT => 4321,
                                ?URL_PATH => <<"/success">>,
                                ?URL_QUERY => <<"a=b">>,
                                ?URL_SCHEME => https,
                                ?USER_AGENT_ORIGINAL => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                                ?HTTP_RESPONSE_STATUS_CODE => 200},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(successful_request)
    end.

custom_client_header_sort(H1, H2) ->
    H1Priority = custom_client_header_priority(H1),
    H2Priority = custom_client_header_priority(H2),

    case {H1Priority, H2Priority} of
        {H1P, H2P} when H1P =< H2P ->
            true;

        {H1P, H2P} when H1P > H2P ->
            false
    end.

custom_client_header_priority({HeaderName, _Value}) ->
    case HeaderName of
        <<"custom-client">> -> 1;
        <<"x-forwarded-for">> -> 2
    end.

custom_server_header_sort(H1, H2) ->
    H1Priority = custom_server_header_priority(H1),
    H2Priority = custom_server_header_priority(H2),

    case {H1Priority, H2Priority} of
        {H1P, H2P} when H1P =< H2P ->
            true;

        {H1P, H2P} when H1P > H2P ->
            false
    end.

custom_server_header_priority({HeaderName, _Value}) ->
    case HeaderName of
        <<"custom-host">> -> 1;
        <<"x-forwarded-host">> -> 2;
        <<"forwarded">> -> 3;
        _ -> 4
    end.

custom_scheme_header_sort(H1, H2) ->
    H1Priority = custom_scheme_header_priority(H1),
    H2Priority = custom_scheme_header_priority(H2),

    case {H1Priority, H2Priority} of
        {H1P, H2P} when H1P =< H2P ->
            true;

        {H1P, H2P} when H1P > H2P ->
            false
    end.

custom_scheme_header_priority({HeaderName, _Value}) ->
    case HeaderName of
        <<"custom-scheme">> -> 1;
        <<"x-forwarded-proto">> -> 2
    end.

successful_request_with_custom_header_setting(_Config) ->
    Opts = #{
        client_address_headers => [<<"x-forwarded-for">>, <<"custom-client">>],
        client_headers_sort_fn => fun ?MODULE:custom_client_header_sort/2,
        scheme_headers => [<<"custom-scheme">>, <<"x-forwarded-proto">>],
        scheme_headers_sort_fn => fun ?MODULE:custom_scheme_header_sort/2,
        server_address_headers => [<<"custom-host">>, <<"forwarded">>, <<"host">>],
        server_headers_sort_fn => fun ?MODULE:custom_server_header_sort/2
    },
    opentelemetry_cowboy:setup(Opts),
    Port = 62661,
    Headers = [
                {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
                {"tracestate", "congo=t61rcWkgMzE"},
                {"x-forwarded-proto", "http"},
                {"custom-scheme", "https"},
                {"custom-client", "23.23.23.23"},
                {"forwarded", "host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=https;by=203.0.113.43"},
                {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
                {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/success", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual('GET', Name),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                                ?CLIENT_ADDRESS => <<"23.23.23.23">>,
                                ?HTTP_REQUEST_METHOD => 'GET',
                                ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                                ?NETWORK_PEER_PORT => Port,
                                ?NETWORK_PROTOCOL_VERSION => '1.1',
                                ?SERVER_ADDRESS => <<"developer.mozilla.org">>,
                                ?SERVER_PORT => 4321,
                                ?URL_PATH => <<"/success">>,
                                ?URL_SCHEME => https,
                                ?USER_AGENT_ORIGINAL => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                                ?HTTP_RESPONSE_STATUS_CODE => 200},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(successful_request)
    end.

chunked_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62651,
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/chunked", []}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            ?assertEqual('GET', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"127.0.0.1">>,
                             ?HTTP_REQUEST_METHOD => 'GET',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/chunked">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<>>,
                             ?HTTP_RESPONSE_STATUS_CODE => 200},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(chunked_request)
    end.

failed_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62652,
    {ok, {{_Version, 500, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/failure", []}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name=exception} = Event,
            ?assertEqual('GET', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"127.0.0.1">>,
                             ?HTTP_REQUEST_METHOD => 'GET',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/failure">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<>>,
                             ?HTTP_RESPONSE_STATUS_CODE => 500,
                             ?ERROR_TYPE => failure},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(failed_request)
    end.

client_timeout_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62653,
    {error, timeout} =
        httpc:request(get, {"http://localhost:8080/slow", []}, [{timeout, 50}], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name='socket_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => closed,
                                   reason => 'The socket has been closed.'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual('GET', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"127.0.0.1">>,
                             ?HTTP_REQUEST_METHOD => 'GET',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/slow">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<>>},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(client_timeout_request)
    end.

idle_timeout_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62654,
    {error, socket_closed_remotely} =
        httpc:request(head, {"http://localhost:8080/slow", []}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name= 'connection_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => timeout,
                                   reason => 'Connection idle longer than configuration allows.'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual('HEAD', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"127.0.0.1">>,
                             ?HTTP_REQUEST_METHOD => 'HEAD',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/slow">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<>>},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(idle_timeout_request)
    end.

chunk_timeout_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62655,
    httpc:request(head, {"http://localhost:8080/chunked_slow", []}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            ?assertEqual('HEAD', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"127.0.0.1">>,
                             ?HTTP_REQUEST_METHOD => 'HEAD',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/chunked_slow">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<>>,
                             ?HTTP_RESPONSE_STATUS_CODE => 200},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(chunk_timeout_request)
    end.

bad_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62656,
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"}],
    {ok, {{_Version, 501, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(trace, {"http://localhost:8080/", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,events=Events,attributes=Attributes,parent_span_id=undefined,kind=Kind}} ->
            [Event] = otel_events:list(Events),
            #event{name='connection_error',attributes = EventAttributes} = Event,
            ExpectedEventAttrs = #{
                                   error => no_error,
                                   reason => 'The TRACE method is currently not implemented. (RFC7231 4.3.8)'
                                  },
            ?assertMatch(ExpectedEventAttrs, otel_attributes:map(EventAttributes)),
            ?assertEqual('HTTP', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ExpectedAttrs = #{?ERROR_TYPE => <<"501">>, ?HTTP_RESPONSE_STATUS_CODE => 501},
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(bad_request)
    end.

binary_status_code_request(_Config) ->
    opentelemetry_cowboy:setup(),
    Port = 62657,
    Headers = [
               {"traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"},
               {"tracestate", "congo=t61rcWkgMzE"},
               {"x-forwarded-for", "203.0.133.195, 70.41.3.18, 150.172.238.178"},
               {"user-agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0"}],
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080/binary_status_code", Headers}, [], [{socket_opts, [{port, Port}]}]),
    receive
        {span, #span{name=Name,attributes=Attributes,parent_span_id=ParentSpanId,kind=Kind}} ->
            ?assertEqual('GET', Name),
            ?assertEqual(?SPAN_KIND_SERVER, Kind),
            ?assertEqual(13235353014750950193, ParentSpanId),
            ExpectedAttrs = #{
                             ?CLIENT_ADDRESS => <<"203.0.133.195">>,
                             ?HTTP_REQUEST_METHOD => 'GET',
                             ?NETWORK_PEER_ADDRESS => <<"127.0.0.1">>,
                             ?NETWORK_PEER_PORT => Port,
                             ?NETWORK_PROTOCOL_VERSION => '1.1',
                             ?SERVER_ADDRESS => <<"localhost">>,
                             ?SERVER_PORT => 8080,
                             ?URL_PATH => <<"/binary_status_code">>,
                             ?URL_SCHEME => http,
                             ?USER_AGENT_ORIGINAL => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0">>,
                             ?HTTP_RESPONSE_STATUS_CODE => 200
                            },
            ?assertMatch(ExpectedAttrs, otel_attributes:map(Attributes))
    after
        1000 -> ct:fail(binary_status_code_request)
    end.
