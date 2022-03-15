-module(otel_elli_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("elli/include/elli.hrl").

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
    application:set_env(opentelemetry_elli, server_name, <<"my-test-elli-server">>),
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
            ?assertMatch(#{<<"http.server_name">> := <<"my-test-elli-server">>,
                           <<"http.target">> := <<"/hello/otel?a=b">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           %% removed until updates to elli allow it
                           %% <<"http.url">> := <<"http://localhost:3000/hello/otel?a=b">>,
                           %% scheme is removed until fixed in elli
                           %% <<"http.scheme">> := <<"http">>,
                           <<"http.status">> := 200,
                           %% <<"http.user_agent">> := <<>>,
                           <<"http.method">> := <<"GET">>,
                           <<"net.host.port">> := 3000}, otel_attributes:map(Attributes)),

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
            ?assertMatch(#{<<"http.server_name">> := <<"my-test-elli-server">>,
                           <<"http.target">> := <<"/hello/otel?a=b">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           %% scheme is removed until fixed in elli
                           %% <<"http.scheme">> := <<"http">>,
                           <<"http.status">> := 200,
                           <<"http.user_agent">> := <<>>,
                           <<"http.method">> := <<"GET">>,
                           <<"net.host.port">> := 3000}, otel_attributes:map(Attributes))
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
            ?assertMatch(#{<<"http.server_name">> := <<"my-test-elli-server">>,
                           <<"http.target">> := <<"/error?a=b">>,
                           <<"http.host">> := <<"localhost:3000">>,
                           <<"http.status">> := 500,
                           <<"http.user_agent">> := <<>>,
                           <<"error.message">> := <<"all_hell">>,
                           <<"http.method">> := <<"GET">>}, otel_attributes:map(Attributes))
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
