-module(opentelemetry_instrumentation_http).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    extract_headers_attributes/3,
    normalize_header_name/1
]).

-spec normalize_header_name(binary()) -> Result when
      Result :: binary() | {error, string(), RestData} | {incomplete, binary(), binary()},
      RestData :: latin1_chardata() | chardata() | external_chardata().
normalize_header_name(Header) when is_binary(Header) ->
    normalize_header_name(binary_to_list(Header));
normalize_header_name(Header) when is_list(Header) ->
    unicode:characters_to_binary(string:replace(string:to_lower(Header), "-", "_", all)).

-spec extract_headers_attributes(
    request | response,
    Headers ::
        #{binary() | string() => binary() | [binary()]}
        | [{binary() | string(), binary() | [binary()]}],
    HeadersToExtract :: [binary()]
) -> #{atom() => [binary()]}.
extract_headers_attributes(_Context, _Headers, []) ->
    #{};
extract_headers_attributes(Context, Headers, HeadersToExtract) when is_list(Headers) ->
    lists:foldr(
        fun({HeaderName, HeaderValue}, Extracted) ->
            extract_if_match(Context, HeaderName, HeaderValue, Extracted, HeadersToExtract)
        end,
        #{},
        Headers
    );
extract_headers_attributes(Context, Headers, HeadersToExtract) when is_map(Headers) ->
    maps:fold(
        fun(HeaderName, HeaderValue, Extracted) ->
            extract_if_match(Context, HeaderName, HeaderValue, Extracted, HeadersToExtract)
        end,
        #{},
        Headers
    ).

extract_if_match(Context, HeaderName, HeaderValue, Extracted, HeadersToExtract) ->
    NormalizedHeaderName = normalize_header_name(HeaderName),
    case lists:member(NormalizedHeaderName, HeadersToExtract) of
        false -> Extracted;
        true -> set_header_attribute(Context, NormalizedHeaderName, HeaderValue, Extracted)
    end.

set_header_attribute(Context, Header, Value, HeadersAttributes) when is_list(Value) ->
    maps:put(attribute_name(Context, Header), Value, HeadersAttributes);
set_header_attribute(Context, Header, Value, HeadersAttributes) ->
    maps:update_with(
        attribute_name(Context, Header), fun(V) -> [Value | V] end, [Value], HeadersAttributes
    ).

attribute_name(request, HeaderName) ->
    binary_to_atom(<<"http.request.header.", HeaderName/binary>>);
attribute_name(response, HeaderName) ->
    binary_to_atom(<<"http.response.header.", HeaderName/binary>>).

-ifdef(TEST).

normalize_header_name_test_() ->
    [
        ?_assertEqual(normalize_header_name(<<"Content-Type">>), <<"content_type">>),
        ?_assertEqual(normalize_header_name("Some-Header-NAME"), <<"some_header_name">>)
    ].

extract_headers_attributes_test_() ->
    [
        ?_assertEqual(extract_headers_attributes(request, [], []), #{}),
        ?_assertEqual(extract_headers_attributes(response, #{}, []), #{}),
        ?_assertEqual(
            extract_headers_attributes(
                request,
                #{
                    <<"Foo">> => <<"1">>,
                    "Bar-Baz" => [<<"2">>, <<"3">>],
                    "To-Not-Extract" => <<"4">>
                },
                [<<"foo">>, <<"bar_baz">>]
            ),
            #{
                'http.request.header.foo' => [<<"1">>],
                'http.request.header.bar_baz' => [<<"2">>, <<"3">>]
            }
        ),
        ?_assertEqual(
            extract_headers_attributes(
                response,
                [
                    {<<"Foo">>, <<"1">>},
                    {"Bar-Baz", <<"2">>},
                    {"To-Not-Extract", <<"3">>},
                    {<<"foo">>, <<"4">>}
                ],
                [<<"foo">>, <<"bar_baz">>]
            ),
            #{
                'http.response.header.foo' => [<<"1">>, <<"4">>],
                'http.response.header.bar_baz' => [<<"2">>]
            }
        )
    ].

-endif.
