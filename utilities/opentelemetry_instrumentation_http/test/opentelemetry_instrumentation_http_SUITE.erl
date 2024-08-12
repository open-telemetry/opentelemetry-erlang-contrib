-module(opentelemetry_instrumentation_http_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        header_name_normalization,
        extract_headers_attributes,
        extracts_client_info_from_headers,
        extracts_scheme_from_headers,
        extracts_server_info_from_headers,
        extracting_ip_and_port_addresses,
        parse_forwarded_headers
    ].

header_name_normalization(_Config) ->
    ?assertEqual(
        opentelemetry_instrumentation_http:normalize_header_name(<<"Content-Type">>),
        <<"content-type">>
    ),
    ?assertEqual(
        opentelemetry_instrumentation_http:normalize_header_name("Some-Header-NAME"),
        <<"some-header-name">>
    ),
    ok.

extract_headers_attributes(_Config) ->
    ?assertEqual(opentelemetry_instrumentation_http:extract_headers_attributes(request, [], []), #{}),
    ?assertEqual(opentelemetry_instrumentation_http:extract_headers_attributes(response, #{}, []), #{}),
    ?assertEqual(
        opentelemetry_instrumentation_http:extract_headers_attributes(
            request,
            #{
                <<"Foo">> => <<"1">>,
                <<"Bar-Baz">> => [<<"2">>, <<"3">>],
                <<"To-Not-Extract">> => <<"4">>
            },
            [<<"foo">>, <<"bar-baz">>]
        ),
        #{
            'http.request.header.foo' => [<<"1">>],
            'http.request.header.bar-baz' => [<<"2">>, <<"3">>]
        }
    ),
    ?assertEqual(
        opentelemetry_instrumentation_http:extract_headers_attributes(
            response,
            [
                {<<"Foo">>, <<"1">>},
                {"Bar-Baz", <<"2">>},
                {"To-Not-Extract", <<"3">>},
                {<<"foo">>, <<"4">>}
            ],
            [<<"foo">>, <<"bar-baz">>]
        ),
        #{
            'http.response.header.foo' => [<<"1">>, <<"4">>],
            'http.response.header.bar-baz' => [<<"2">>]
        }
    ),
    ok.

parse_forwarded_headers(_Config) ->
    ?assertEqual(
        #{
            <<"host">> => [<<"developer.mozilla.org:4321">>],
            <<"for">> => [<<"192.0.2.60">>, <<"\"[2001:db8:cafe::17]\"">>],
            <<"proto">> => [<<"http">>],
            <<"by">> => [<<"203.0.113.43">>]
        },
        opentelemetry_instrumentation_http:parse_forwarded_header(
            <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
        )
    ).

extracting_ip_and_port_addresses(_Config) ->
    ?assertEqual(
        {"192.0.2.60", undefined},
        opentelemetry_instrumentation_http:extract_ip_port(<<"192.0.2.60">>)
    ),
    ?assertEqual(
        {"192.0.2.60", 443},
        opentelemetry_instrumentation_http:extract_ip_port(<<"192.0.2.60:443">>)
    ),
    ?assertEqual(
        {"192.0.2.60", undefined},
        opentelemetry_instrumentation_http:extract_ip_port(<<"192.0.2.60:junk">>)
    ),
    ?assertEqual(
        {"2001:db8:cafe::17", undefined},
        opentelemetry_instrumentation_http:extract_ip_port(<<"\"[2001:db8:cafe::17]\"">>)
    ),
    ?assertEqual(
        {"2001:db8:cafe::17", 8000},
        opentelemetry_instrumentation_http:extract_ip_port(<<"\"[2001:db8:cafe::17]:8000\"">>)
    ),
    ?assertEqual(
        {"::", undefined},
        opentelemetry_instrumentation_http:extract_ip_port(<<"\"[::]:99999\"">>)
    ),
    ?assertEqual(
        {"2001:db8:cafe::17", undefined},
        opentelemetry_instrumentation_http:extract_ip_port(<<"\"[2001:db8:cafe::17]:junk\"">>)
    ).

extracts_client_info_from_headers(_Config) ->
    ?assertEqual(
        #{ip => "192.0.2.60", port => undefined},
        opentelemetry_instrumentation_http:extract_client_info(#{
            <<"forwarded">> =>
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
        })
    ),
    ?assertEqual(
        #{ip => "2001:db8:cafe::17", port => undefined},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org:4321; for=\"[2001:db8:cafe::17]\", for=192.0.2.60; proto=http;by=203.0.113.43">>}
        ])
    ),
    ?assertEqual(
        #{ip => "2001:db8:cafe::17", port => 9678},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org:4321;for=\"[2001:db8:cafe::17]:9678\",for=192.0.2.60;proto=http;by=203.0.113.43">>}
        ])
    ),
    ?assertEqual(
        #{ip => "23.0.2.1", port => 2121},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"x-forwarded-for">>, <<"23.0.2.1:2121">>}
        ])
    ),
    ?assertEqual(
        #{ip => "192.0.2.60", port => undefined},
        opentelemetry_instrumentation_http:extract_client_info(#{
            <<"x-forwarded-for">> => <<"23.0.2.1:2121">>,
            <<"forwarded">> =>
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
        })
    ),
    ?assertEqual(
        #{ip => "192.0.2.60", port => undefined},
        opentelemetry_instrumentation_http:extract_client_info(#{
            <<"forwarded">> =>
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>,
            <<"x-forwarded-for">> => <<"23.0.2.1:2121">>
        })
    ),
    ?assertEqual(
        #{ip => "23.0.2.1", port => 2121},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"x-forwarded-for">>, <<"23.0.2.1:2121,10.100.10.10">>},
            {<<"forwarded">>,
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>}
        ])
    ),
    ?assertEqual(
        #{ip => "192.0.2.60", port => undefined},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>},
            {<<"x-forwarded-for">>, <<"23.0.2.1:2121,10.100.10.10">>}
        ])
    ),
    ?assertEqual(
        #{ip => "27.27.27.27", port => 2222},
        opentelemetry_instrumentation_http:extract_client_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>},
            {<<"x-forwarded-for">>, <<"23.0.2.1:2121">>},
            {<<"x-real-client-ip">>, <<"27.27.27.27:2222">>}
        ],
        fun(Header1, _Header2) ->
          Header1 == <<"x-real-client-ip">>
        end)
    ).


extracts_server_info_from_headers(_Config) ->
    ?assertEqual(
        #{address => <<"developer.mozilla.org">>, port => 4321},
        opentelemetry_instrumentation_http:extract_server_info(#{
            <<"forwarded">> =>
                <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
        })
    ),
    ?assertEqual(
        #{address => <<"developer.mozilla.org">>, port => undefined},
        opentelemetry_instrumentation_http:extract_server_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
        }])
    ),
    ?assertEqual(
        #{address => <<"d1.mozilla.org">>, port => undefined},
        opentelemetry_instrumentation_http:extract_server_info([
            {<<"host">>, <<"d1.mozilla.org">>},
            {<<"forwarded">>,
                <<"host=developer.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>}
        ])
    ),
    ?assertEqual(
        #{address => <<"developer.mozilla.org">>, port => undefined},
        opentelemetry_instrumentation_http:extract_server_info([
            {<<"x-forwarded-host">>, <<"developer.mozilla.org">>},
            {<<"forwarded">>,
                <<"host=d1.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>}
        ])
    ),
    ?assertEqual(
        #{address => <<"developer.mozilla.org">>, port => undefined},
        opentelemetry_instrumentation_http:extract_server_info([
            {<<"forwarded">>,
                <<"host=developer.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>},
            {<<"host">>, <<"d1.mozilla.org">>}
        ])
    ).

    extracts_scheme_from_headers(_Config) ->
        ?assertEqual(
            http,
            opentelemetry_instrumentation_http:extract_scheme(#{
                <<"forwarded">> =>
                    <<"host=developer.mozilla.org:4321; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
            })
        ),
        ?assertEqual(
            http,
            opentelemetry_instrumentation_http:extract_scheme([
                {<<"forwarded">>,
                    <<"host=developer.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>
            }])
        ),
        ?assertEqual(
            https,
            opentelemetry_instrumentation_http:extract_scheme([
                {<<"x-forwarded-proto">>, <<"https">>},
                {<<"forwarded">>,
                    <<"host=developer.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>}
            ])
        ),
        ?assertEqual(
            https,
            opentelemetry_instrumentation_http:extract_scheme([
                {<<":scheme">>, <<"https">>},
                {<<"forwarded">>,
                    <<"host=d1.mozilla.org; for=192.0.2.60, for=\"[2001:db8:cafe::17]\";proto=http;by=203.0.113.43">>}
            ])
        ).
    