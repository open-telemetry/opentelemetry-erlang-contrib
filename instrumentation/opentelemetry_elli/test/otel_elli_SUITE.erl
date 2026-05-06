-module(otel_elli_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("elli/include/elli.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/client_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/error_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/network_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/server_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/user_agent_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/http_attributes.hrl").

all() ->
    [{group, w3c}, {group, b3multi}, {group, b3}].

groups() ->
    [{w3c, [shuffle], [successful_request_no_parent, successful_request,
                       error_response, excluded_paths]},
     {b3, [shuffle], [successful_request_no_parent, successful_request,
                      error_response, excluded_paths]},
     {b3multi, [shuffle], [successful_request_no_parent, successful_request,
                           error_response, excluded_paths]}].

init_per_suite(Config) ->
    ok = application:load(opentelemetry_elli),
    ok = application:load(opentelemetry),

    application:set_env(opentelemetry_elli, excluded_paths, ["/hello/exclude"]),
    application:set_env(opentelemetry, processors, [{otel_simple_processor, #{}}]),

    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_group(Propagator, Config) ->
    {ok, _} = application:ensure_all_started(opentelemetry),

    Propagators = case Propagator of
                      w3c ->
                          [baggage, trace_context];
                      b3multi ->
                          [baggage, b3multi];
                      b3 ->
                          [baggage, b3]
                  end,
    CompositePropagator = otel_propagator_text_map_composite:create(Propagators),
    opentelemetry:set_text_map_propagator(CompositePropagator),

    [{propagator, Propagator} | Config].

end_per_group(_, _Config)->
    application:stop(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    elli:start_link([{name, {local, elli_test_server}},
                     {port, 3000},
                     {callback, elli_middleware},
                     {callback_args, [{mods, [{otel_elli_middleware, []},
                                              {?MODULE, []}]}]}]),

    otel_simple_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_, _Config) ->
    elli:stop(elli_test_server),
    ok.

excluded_paths(_Config) ->
    ?with_span(<<"remote-parent">>, #{},
               fun(_) ->
                       RequestHeaders = [{binary_to_list(K), binary_to_list(V)}
                                         || {K, V} <- otel_propagator_text_map:inject([])],
                       {ok, {{_, 200, _}, _Headers, Body}} =
                           httpc:request(get, {"http://localhost:3000/hello/exclude",
                                               RequestHeaders},
                                         [], []),
                       ?assertEqual("Hello exclude", Body)
               end),

    receive
        {span, #span{name=Name,
                     parent_span_id=Parent}} when Parent =/= undefined ->
            ?assertEqual(<<"handler-child">>, Name),

            %% then receive the remote parent
            receive
                {span, #span{name=ParentName,
                             span_id=ParentSpanId,
                             parent_span_id=undefined}} when ParentSpanId =:= Parent ->
                    ?assertEqual(<<"remote-parent">>, ParentName),

                    %% and guarantee that the mailbox is empty, meaning no elli_middleware span
                    receive
                        _ ->
                            ct:fail(mailbox_not_empty)
                    after
                        0 ->
                            ok
                    end
            after
                5000 ->
                    ct:fail(timeout)
            end
    after
        5000 ->
            ct:fail(timeout)
    end,

    ok.

successful_request(_Config) ->
    ?with_span(<<"remote-parent">>, #{},
               fun(_) ->
                       RequestHeaders = [{binary_to_list(K), binary_to_list(V)}
                                         || {K, V} <- otel_propagator_text_map:inject([])],
                       {ok, {{_, 200, _}, _Headers, Body}} =
                           httpc:request(get, {"http://localhost:3000/hello/otel?a=b#fragment",
                                               RequestHeaders},
                                         [], []),
                       ?assertEqual("Hello otel", Body)
               end),

    receive
        {span, #span{name = <<"/hello/{who}">>,
                     parent_span_id=Parent,
                     attributes=Attributes,
                     events=_TimeEvents}} when Parent =/= undefined ->
            ?assertMatch(#{?URL_PATH := <<"/hello/otel">>,
                           ?URL_QUERY := <<"a=b">>,
                           ?URL_SCHEME := http,
                           ?SERVER_ADDRESS := <<"localhost">>,
                           ?SERVER_PORT := 3000,
                           ?HTTP_RESPONSE_STATUS_CODE := 200,
                           ?HTTP_REQUEST_METHOD := 'GET',
                           ?NETWORK_PEER_PORT := _,
                           ?NETWORK_PEER_ADDRESS := _}, otel_attributes:map(Attributes)),

            %% then receive the remote parent
            receive
                {span, #span{name=ParentName,
                             span_id=ParentSpanId,
                             parent_span_id=undefined}} when ParentSpanId =:= Parent ->
                    ?assertEqual(<<"remote-parent">>, ParentName)
            after
                5000 ->
                    ct:fail(timeout)
            end
    after
        5000 ->
            ct:fail(timeout)
    end,

    ok.

successful_request_no_parent(_Config) ->
    {ok, {{_, 200, _}, _Headers, Body}} = httpc:request("http://localhost:3000/hello/otel?a=b#fragment"),
    ?assertEqual("Hello otel", Body),

    receive
        {span, #span{name = <<"/hello/{who}">>,
                     parent_span_id=Parent,
                     attributes=Attributes,
                     events=_TimeEvents}} ->
            ?assertEqual(undefined, Parent),
            ?assertMatch(#{?URL_PATH := <<"/hello/otel">>,
                           ?URL_QUERY := <<"a=b">>,
                           ?URL_SCHEME := http,
                           ?SERVER_ADDRESS := <<"localhost">>,
                           ?SERVER_PORT := 3000,
                           ?HTTP_RESPONSE_STATUS_CODE := 200,
                           ?USER_AGENT_ORIGINAL := <<>>,
                           ?HTTP_REQUEST_METHOD := 'GET',
                           ?NETWORK_PEER_PORT := _,
                           ?NETWORK_PEER_ADDRESS := _}, otel_attributes:map(Attributes))
    after
        5000 ->
            ct:fail(timeout)
    end,
    ok.

error_response(_Config) ->
    {ok, {{_, 500, _}, _Headers, _Body}} = httpc:request("http://localhost:3000/error?a=b#fragment"),

    receive
        {span, #span{name=Name,
                     parent_span_id=Parent,
                     attributes=Attributes}} ->
            ?assertEqual(undefined, Parent),
            ?assertEqual(<<"HTTP GET">>, Name),
            ?assertMatch(#{?URL_PATH := <<"/error">>,
                           ?URL_QUERY := <<"a=b">>,
                           ?SERVER_ADDRESS := <<"localhost">>,
                           ?SERVER_PORT := 3000,
                           ?HTTP_RESPONSE_STATUS_CODE := 500,
                           ?USER_AGENT_ORIGINAL := <<>>,
                           ?ERROR_TYPE := _,
                           ?HTTP_REQUEST_METHOD := 'GET'}, otel_attributes:map(Attributes))
    after
        5000 ->
            ct:fail(timeout)
    end,
    ok.
%%

handle(Req, Args) ->
    handle(Req#req.path, Req, Args).

handle([<<"hello">>, Who], _Req, _Args) ->
    ?update_name(<<"/hello/{who}">>),

    ?with_span(<<"handler-child">>, #{},
               fun(_) ->
                       ok
               end),

    {ok, [], <<"Hello ", Who/binary>>};
handle([<<"error">>], _Req, _Args) ->
    throw(all_hell).

handle_event(_Event, _Data, _Args) ->
    ok.
