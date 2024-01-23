-module(opentelemetry_instrumentation_http).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    code/1,
    code_to_atom/1,
    code_to_phrase/1,
    extract_headers_attributes/3,
    normalize_header_name/1
]).

-spec normalize_header_name(string() | binary()) -> Result when
      Result :: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()},
      RestData :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata().
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

-spec code(atom()) -> integer().
code(continue) -> 100;
code(switching_protocols) -> 101;
code(processing) -> 102;
code(early_hints) -> 103;
code(ok) -> 200;
code(created) -> 201;
code(accepted) -> 202;
code(non_authoritative_information) -> 203;
code(no_content) -> 204;
code(reset_content) -> 205;
code(partial_content) -> 206;
code(multi_status) -> 207;
code(already_reported) -> 208;
code(im_used) -> 226;
code(multiple_choices) -> 300;
code(moved_permanently) -> 301;
code(found) -> 302;
code(see_other) -> 303;
code(not_modified) -> 304;
code(use_proxy) -> 305;
code(switch_proxy) -> 306;
code(temporary_redirect) -> 307;
code(permanent_redirect) -> 308;
code(bad_request) -> 400;
code(unauthorized) -> 401;
code(payment_required) -> 402;
code(forbidden) -> 403;
code(not_found) -> 404;
code(method_not_allowed) -> 405;
code(not_acceptable) -> 406;
code(proxy_authentication_required) -> 407;
code(request_timeout) -> 408;
code(conflict) -> 409;
code(gone) -> 410;
code(length_required) -> 411;
code(precondition_failed) -> 412;
code(request_entity_too_large) -> 413;
code(request_uri_too_long) -> 414;
code(unsupported_media_type) -> 415;
code(requested_range_not_satisfiable) -> 416;
code(expectation_failed) -> 417;
code(im_a_teapot) -> 418;
code(misdirected_request) -> 421;
code(unprocessable_entity) -> 422;
code(locked) -> 423;
code(failed_dependency) -> 424;
code(too_early) -> 425;
code(upgrade_required) -> 426;
code(precondition_required) -> 428;
code(too_many_requests) -> 429;
code(request_header_fields_too_large) -> 431;
code(unavailable_for_legal_reasons) -> 451;
code(internal_server_error) -> 500;
code(not_implemented) -> 501;
code(bad_gateway) -> 502;
code(service_unavailable) -> 503;
code(gateway_timeout) -> 504;
code(http_version_not_supported) -> 505;
code(variant_also_negotiates) -> 506;
code(insufficient_storage) -> 507;
code(loop_detected) -> 508;
code(not_extended) -> 510;
code(network_authentication_required) -> 511.

-spec code_to_atom(integer()) -> atom().
code_to_atom(100) -> continue;
code_to_atom(101) -> switching_protocols;
code_to_atom(102) -> processing;
code_to_atom(103) -> early_hints;
code_to_atom(200) -> ok;
code_to_atom(201) -> created;
code_to_atom(202) -> accepted;
code_to_atom(203) -> non_authoritative_information;
code_to_atom(204) -> no_content;
code_to_atom(205) -> reset_content;
code_to_atom(206) -> partial_content;
code_to_atom(207) -> multi_status;
code_to_atom(208) -> already_reported;
code_to_atom(226) -> im_used;
code_to_atom(300) -> multiple_choices;
code_to_atom(301) -> moved_permanently;
code_to_atom(302) -> found;
code_to_atom(303) -> see_other;
code_to_atom(304) -> not_modified;
code_to_atom(305) -> use_proxy;
code_to_atom(306) -> switch_proxy;
code_to_atom(307) -> temporary_redirect;
code_to_atom(308) -> permanent_redirect;
code_to_atom(400) -> bad_request;
code_to_atom(401) -> unauthorized;
code_to_atom(402) -> payment_required;
code_to_atom(403) -> forbidden;
code_to_atom(404) -> not_found;
code_to_atom(405) -> method_not_allowed;
code_to_atom(406) -> not_acceptable;
code_to_atom(407) -> proxy_authentication_required;
code_to_atom(408) -> request_timeout;
code_to_atom(409) -> conflict;
code_to_atom(410) -> gone;
code_to_atom(411) -> length_required;
code_to_atom(412) -> precondition_failed;
code_to_atom(413) -> request_entity_too_large;
code_to_atom(414) -> request_uri_too_long;
code_to_atom(415) -> unsupported_media_type;
code_to_atom(416) -> requested_range_not_satisfiable;
code_to_atom(417) -> expectation_failed;
code_to_atom(418) -> im_a_teapot;
code_to_atom(421) -> misdirected_request;
code_to_atom(422) -> unprocessable_entity;
code_to_atom(423) -> locked;
code_to_atom(424) -> failed_dependency;
code_to_atom(425) -> too_early;
code_to_atom(426) -> upgrade_required;
code_to_atom(428) -> precondition_required;
code_to_atom(429) -> too_many_requests;
code_to_atom(431) -> request_header_fields_too_large;
code_to_atom(451) -> unavailable_for_legal_reasons;
code_to_atom(500) -> internal_server_error;
code_to_atom(501) -> not_implemented;
code_to_atom(502) -> bad_gateway;
code_to_atom(503) -> service_unavailable;
code_to_atom(504) -> gateway_timeout;
code_to_atom(505) -> http_version_not_supported;
code_to_atom(506) -> variant_also_negotiates;
code_to_atom(507) -> insufficient_storage;
code_to_atom(508) -> loop_detected;
code_to_atom(510) -> not_extended;
code_to_atom(511) -> network_authentication_required.

-spec code_to_phrase(integer()) -> binary().
code_to_phrase(100) -> <<"Continue">>;
code_to_phrase(101) -> <<"Switching Protocols">>;
code_to_phrase(102) -> <<"Processing">>;
code_to_phrase(103) -> <<"Early Hints">>;
code_to_phrase(200) -> <<"OK">>;
code_to_phrase(201) -> <<"Created">>;
code_to_phrase(202) -> <<"Accepted">>;
code_to_phrase(203) -> <<"Non-Authoritative Information">>;
code_to_phrase(204) -> <<"No Content">>;
code_to_phrase(205) -> <<"Reset Content">>;
code_to_phrase(206) -> <<"Partial Content">>;
code_to_phrase(207) -> <<"Multi-Status">>;
code_to_phrase(208) -> <<"Already Reported">>;
code_to_phrase(226) -> <<"IM Used">>;
code_to_phrase(300) -> <<"Multiple Choices">>;
code_to_phrase(301) -> <<"Moved Permanently">>;
code_to_phrase(302) -> <<"Found">>;
code_to_phrase(303) -> <<"See Other">>;
code_to_phrase(304) -> <<"Not Modified">>;
code_to_phrase(305) -> <<"Use Proxy">>;
code_to_phrase(306) -> <<"Switch Proxy">>;
code_to_phrase(307) -> <<"Temporary Redirect">>;
code_to_phrase(308) -> <<"Permanent Redirect">>;
code_to_phrase(400) -> <<"Bad Request">>;
code_to_phrase(401) -> <<"Unauthorized">>;
code_to_phrase(402) -> <<"Payment Required">>;
code_to_phrase(403) -> <<"Forbidden">>;
code_to_phrase(404) -> <<"Not Found">>;
code_to_phrase(405) -> <<"Method Not Allowed">>;
code_to_phrase(406) -> <<"Not Acceptable">>;
code_to_phrase(407) -> <<"Proxy Authentication Required">>;
code_to_phrase(408) -> <<"Request Timeout">>;
code_to_phrase(409) -> <<"Conflict">>;
code_to_phrase(410) -> <<"Gone">>;
code_to_phrase(411) -> <<"Length Required">>;
code_to_phrase(412) -> <<"Precondition Failed">>;
code_to_phrase(413) -> <<"Request Entity Too Large">>;
code_to_phrase(414) -> <<"Request-URI Too Long">>;
code_to_phrase(415) -> <<"Unsupported Media Type">>;
code_to_phrase(416) -> <<"Requested Range Not Satisfiable">>;
code_to_phrase(417) -> <<"Expectation Failed">>;
code_to_phrase(418) -> <<"I'm a teapot">>;
code_to_phrase(421) -> <<"Misdirected Request">>;
code_to_phrase(422) -> <<"Unprocessable Entity">>;
code_to_phrase(423) -> <<"Locked">>;
code_to_phrase(424) -> <<"Failed Dependency">>;
code_to_phrase(425) -> <<"Too Early">>;
code_to_phrase(426) -> <<"Upgrade Required">>;
code_to_phrase(428) -> <<"Precondition Required">>;
code_to_phrase(429) -> <<"Too Many Requests">>;
code_to_phrase(431) -> <<"Request Header Fields Too Large">>;
code_to_phrase(451) -> <<"Unavailable For Legal Reasons">>;
code_to_phrase(500) -> <<"Internal Server Error">>;
code_to_phrase(501) -> <<"Not Implemented">>;
code_to_phrase(502) -> <<"Bad Gateway">>;
code_to_phrase(503) -> <<"Service Unavailable">>;
code_to_phrase(504) -> <<"Gateway Timeout">>;
code_to_phrase(505) -> <<"HTTP Version Not Supported">>;
code_to_phrase(506) -> <<"Variant Also Negotiates">>;
code_to_phrase(507) -> <<"Insufficient Storage">>;
code_to_phrase(508) -> <<"Loop Detected">>;
code_to_phrase(510) -> <<"Not Extended">>;
code_to_phrase(511) -> <<"Network Authentication Required">>.

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
