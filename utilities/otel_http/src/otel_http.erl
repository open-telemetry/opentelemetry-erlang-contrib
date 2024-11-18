-module(otel_http).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-export([
    extract_headers_attributes/3,
    extract_client_info/1,
    extract_client_info/2,
    extract_ip_port/1,
    extract_scheme/1,
    extract_scheme/2,
    extract_server_info/1,
    extract_server_info/2,
    normalize_header_name/1,
    parse_forwarded_header/1
]).


-type client_info() :: #{ip => binary() | undefined, port => integer()}.
-type header_name() :: binary() | string().
-type header_value() :: binary().
-type header() :: {header_name(), header_value()}.
-type headers_map() :: #{header_name() => header_value()}.
-type header_sort_fun() :: fun((Header1 :: header_name(), Header2 :: header_name()) -> boolean()).
-type server_info() :: #{address => binary() | undefined, port => integer() | undefined}.

-export_type([
              client_info/0,
              header_name/0,
              header_value/0,
              headers_map/0,
              header_sort_fun/0,
              server_info/0]).

?MODULEDOC("""
`otel_http` provides utility functions for
common otel http-related instrumentation operations such as extraction
of schemes, client and server info, and header operations.
""").

?DOC("""
Extract the original client request protocol scheme from request headers.

For lists of headers, the scheme is extracted from the first header
which can contain the scheme. For maps of headers, the map is converted
to a list and a sort function applied according to the semantic convention's
priority order. See the documentation for `extract_scheme/2` for more
information.
""").

-spec extract_scheme([header()] | headers_map()) -> http | https | undefined.
extract_scheme([]) ->
    undefined;
extract_scheme(Map) when is_map(Map) and map_size(Map) == 0 ->
    undefined;
extract_scheme(Headers) when is_list(Headers) ->
    extract_scheme(Headers, fun list_scheme_headers_sort/2);
extract_scheme(Headers) when is_map(Headers) ->
    extract_scheme(maps:to_list(Headers), fun map_scheme_headers_sort/2).

?DOC("""
Extract the original client request protocol scheme from request headers.
Users may supply their own sort function for prioritizing a particular header
over others.

The default sort order per SemConv gives equal priority to `forwarded`
and `x-forwarded-proto` headers, followed by the `:scheme` HTTP2 header.
""").
-spec extract_scheme(Headers, Fun) -> http | https | undefined
  when Fun :: header_sort_fun(), Headers :: [header()].
extract_scheme(Headers, SortFun) when is_list(Headers) ->
    SortedHeaders = lists:sort(SortFun, Headers),
    reduce_while(
        SortedHeaders, undefined, fun(Header, Acc) ->
            case extract_scheme_from_header(Header) of
                undefined ->
                    {cont, Acc};
                <<"http">> ->
                    {halt, http};
                <<"https">> ->
                    {halt, https};
                _ ->
                    {halt, undefined}
            end
        end
    ).

list_scheme_headers_sort(_, _) -> true.

map_scheme_headers_sort(HeaderName1, HeaderName2) ->
    HN1Priority = scheme_header_priority(HeaderName1),
    HN2Priority = scheme_header_priority(HeaderName2),
    case {HN1Priority, HN2Priority} of
        {H1, H2} when H1 =< H2 ->
            true;
        {H1, H2} when H1 > H2 ->
            false
    end.

% Define header priority
scheme_header_priority({HeaderName, _Value}) ->
    Priority =
        case HeaderName of
            <<"forwarded">> -> 1;
            <<"x-forwarded-proto">> -> 1;
            <<":scheme">> -> 2;
            % Default priority for other headers
            _ -> 4
        end,
    Priority.

extract_scheme_from_header({<<"forwarded">>, Value}) ->
    DirectiveMap = parse_forwarded_header(Value),
    SchemeValue = maps:get(<<"proto">>, DirectiveMap, []),
    case SchemeValue of
        [] ->
            undefined;
        [Scheme | _Rest] ->
            Scheme;
        _ ->
            undefined
    end;
extract_scheme_from_header({_Other, Value}) ->
    Value.

?DOC("""
Extract the original client request protocol scheme from request headers.

For lists of headers, the address and port are extracted from the first header
which can contain that vbalue. For maps of headers, the map is converted
to a list and a sort function applied according to the semantic convention's
priority order. See the documentation for `extract_server_info/2` for more
information.
""").

-spec extract_server_info([header()] | headers_map()) -> server_info().
extract_server_info([]) ->
  #{address => undefined, port => undefined};
extract_server_info(Map) when is_map(Map) and map_size(Map) == 0 ->
  #{address => undefined, port => undefined};
extract_server_info(Headers) when is_list(Headers) ->
    extract_server_info(Headers, fun list_server_headers_sort/2);
extract_server_info(Headers) when is_map(Headers) ->
    extract_server_info(maps:to_list(Headers), fun map_server_headers_sort/2).

?DOC("""
Extract the original server address and port from request headers.
Users may supply their own sort function for prioritizing a particular header
over others.

The default sort order per SemConv gives equal priority to `forwarded`
and `x-forwarded-host` headers, followed by the `:authority` HTTP2 header
and then `host`.
""").
-spec extract_server_info(Headers, Fun) -> server_info()
  when Fun :: header_sort_fun(), Headers :: [header()].
extract_server_info(Headers, SortFun) ->
    SortedHeaders = lists:sort(SortFun, Headers),
    reduce_while(
        SortedHeaders, #{address => undefined, port => undefined}, fun(Header, Acc) ->
            case extract_server_info_from_header(Header) of
                {undefined, undefined} ->
                    {cont, Acc};
                {Address, Port} ->
                    {halt, #{address => Address, port => Port}}
            end
        end
    ).

list_server_headers_sort(_, _) -> true.

map_server_headers_sort(HeaderName1, HeaderName2) ->
    HN1Priority = server_header_priority(HeaderName1),
    HN2Priority = server_header_priority(HeaderName2),
    case {HN1Priority, HN2Priority} of
        {H1, H2} when H1 =< H2 ->
            true;
        {H1, H2} when H1 > H2 ->
            false
    end.

server_header_priority({HeaderName, _Value}) ->
    Priority =
        case HeaderName of
            <<"forwarded">> -> 1;
            <<"x-forwarded-host">> -> 1;
            <<":authority">> -> 2;
            <<"host">> -> 3;
            % Default priority for other headers
            _ -> 4
        end,
    Priority.

extract_server_info_from_header({<<"forwarded">>, Value}) ->
    DirectiveMap = parse_forwarded_header(Value),
    HostValue = maps:get(<<"host">>, DirectiveMap, []),
    case HostValue of
        [] ->
            {undefined, undefined};
        [LeftMostHost | _Rest] ->
            case string:split(LeftMostHost, ":") of
                [Host] ->
                    {Host, undefined};
                [Host, Port] ->
                    {Host, extract_port(Port)};
                _ ->
                    {undefined, undefined}
            end
    end;
extract_server_info_from_header({_Other, Value}) ->
    case string:split(Value, ":") of
        [Host] ->
            {Host, undefined};
        [Host, Port] ->
            {Host, extract_port(Port)};
        _ ->
            {undefined, undefined}
    end.

?DOC("""
Extract the original client request information from request headers.

For lists of headers, the ip and port are extracted from the first header
which can contain the scheme. For maps of headers, the map is converted
to a list and a sort function applied according to the semantic convention's
priority order. See the documentation for `extract_client_info/2` for more
information.
""").
-spec extract_client_info([header()] | headers_map()) -> client_info().
extract_client_info([]) ->
  #{ip => undefined, port => undefined};
extract_client_info(Map) when is_map(Map) and map_size(Map) == 0 ->
  #{ip => undefined, port => undefined};
extract_client_info(Headers) when is_list(Headers) ->
    extract_client_info(Headers, fun list_client_headers_sort/2);
extract_client_info(Headers) when is_map(Headers) ->
    extract_client_info(maps:to_list(Headers), fun map_client_headers_sort/2).

?DOC("""
Extract the original server address and port from request headers.
Users may supply their own sort function for prioritizing a particular header
over others.

The default sort order per SemConv gives equal priority to `forwarded`
and `x-forwarded-for` headers.
""").
-spec extract_client_info(Headers, Fun) -> server_info()
  when Fun :: header_sort_fun(), Headers :: [header()].
extract_client_info(Headers, SortFun) ->
    SortedHeaders = lists:sort(SortFun, Headers),
    reduce_while(
        SortedHeaders, #{ip => undefined, port => undefined}, fun(Header, Acc) ->
            case extract_client_info_from_header(Header) of
                {undefined, undefined} ->
                    {cont, Acc};
                {Ip, Port} ->
                    {halt, #{ip => Ip, port => Port}}
            end
        end
    ).

extract_client_info_from_header({<<"forwarded">>, Value}) ->
    DirectiveMap = parse_forwarded_header(Value),
    ForValue = maps:get(<<"for">>, DirectiveMap, []),
    case ForValue of
        [] ->
            {undefined, undefined};
        [LeftMostFor | _Rest] ->
            extract_ip_port(LeftMostFor)
    end;
extract_client_info_from_header({_Other, Value}) ->
    extract_ip_port(Value).

map_client_headers_sort(HeaderName1, HeaderName2) ->
    HN1Priority = client_header_priority(HeaderName1),
    HN2Priority = client_header_priority(HeaderName2),
    case {HN1Priority, HN2Priority} of
        {H1, H2} when H1 =< H2 ->
            true;
        {H1, H2} when H1 > H2 ->
            false
    end.

list_client_headers_sort(_, _) -> true.

% Define header priority
client_header_priority({HeaderName, _Value}) ->
    Priority =
        case HeaderName of
            <<"forwarded">> -> 1;
            <<"x-forwarded-for">> -> 1;
            % Default priority for other headers
            _ -> 4
        end,
    Priority.

?DOC("""
Parse a `forwarded` header to a map of directives.
""").
-spec parse_forwarded_header(header()) ->
  #{binary() => [header_value()]}.
parse_forwarded_header(Header) ->
    KvpList = string:split(Header, <<";">>, all),
    Grouped = lists:foldl(fun group_by/2, #{}, KvpList),
    Grouped.

group_by(Kvp, Acc) ->
    SplitDirectives = string:split(Kvp, <<",">>, all),
    lists:foldr(
        fun(ProcessedKvp, A) ->
            case string:split(string:trim(ProcessedKvp), <<"=">>, all) of
                [Directive, Value] ->
                    TrimmedValue = string:trim(Value),
                    update_group(string:trim(Directive), TrimmedValue, A);
                _Malformed ->
                    A
            end
        end,
        Acc,
        SplitDirectives
    ).

update_group(Key, Value, Acc) ->
    case maps:get(Key, Acc, []) of
        List -> Acc#{Key => [Value | List]}
    end.

extract_ip_port(IpStr) when is_binary(IpStr) ->
    extract_ip_port(binary_to_list(IpStr));
extract_ip_port(IpStr) when is_list(IpStr) ->
    case re:split(IpStr, "[\]\[/\/\"]", [{return, list}, trim]) of
        [[], [], Ip] ->
            case inet:parse_ipv6strict_address(Ip) of
                {ok, IpV6} ->
                    {list_to_binary(inet:ntoa(IpV6)), undefined};
                _ ->
                    {undefined, undefined}
            end;
        [[], [], Ip, Port] ->
            case inet:parse_ipv6strict_address(Ip) of
                {ok, IpV6} ->
                    {list_to_binary(inet:ntoa(IpV6)), extract_port(string:trim(Port, leading, ":"))};
                _ ->
                    {undefined, undefined}
            end;
        [IpV4Str] ->
            [LeftMostIpV4Str | _] = string:split(IpV4Str, <<",">>),
            case string:split(LeftMostIpV4Str, ":") of
                [Ip, Port] ->
                    case inet:parse_ipv4strict_address(Ip) of
                        {ok, IpV4} ->
                            {list_to_binary(inet:ntoa(IpV4)), extract_port(Port)};
                        _ ->
                            {undefined, undefined}
                    end;
                [Ip] ->
                    case inet:parse_ipv4strict_address(Ip) of
                        {ok, IpV4} ->
                            {list_to_binary(inet:ntoa(IpV4)), undefined};
                        _ ->
                            {undefined, undefined}
                    end;
                _ ->
                    {undefined, undefined}
            end;
        _Other ->
            {undefined, undefined}
    end;
extract_ip_port(_) ->
    {undefined, undefined}.

extract_port(PortStr) ->
    case string:to_integer(PortStr) of
        {error, _} ->
            undefined;
        {Port, _} when Port =< 65535 ->
            Port;
        _ ->
            undefined
    end.

reduce_while([], Acc, _Fun) ->
    Acc;
reduce_while([H | T], Acc, Fun) ->
    case Fun(H, Acc) of
        {halt, NewAcc} -> NewAcc;
        {cont, NewAcc} -> reduce_while(T, NewAcc, Fun)
    end.

?DOC("""
Normalizes a header name to a lowercase binary.
""").
-spec normalize_header_name(string() | binary()) -> Result when
    Result :: binary() | {error, binary(), RestData} | {incomplete, binary(), binary()},
    RestData :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata().
normalize_header_name(Header) when is_binary(Header) ->
    normalize_header_name(binary_to_list(Header));
normalize_header_name(Header) when is_list(Header) ->
    unicode:characters_to_binary(string:to_lower(Header)).

?DOC("""
Extracts a map of request or response header attributes for a given
set of attributes and list of headers to extract.

NOTE: keys are generated as atoms for efficiency purposes so take care
that header names have a defined cardinality set. These values should never
be dynamic.
""").
-spec extract_headers_attributes(
    request | response,
    Headers ::
        #{header_name() => header_value() | [header_value()]}
        | [{header_name(), header_value() | [header_value()]}],
    HeadersToExtract :: [header_name()]
) -> #{atom() => [header_value()]}.
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
