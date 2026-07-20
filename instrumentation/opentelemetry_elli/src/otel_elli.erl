-module(otel_elli).

-export([start_span/1]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("elli/include/elli.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/client_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/error_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/network_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/server_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/url_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/attributes/user_agent_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/http_attributes.hrl").

start_span(Req) ->
    Method = elli_request:method(Req),
    RawPath = elli_request:raw_path(Req),

    BinMethod = to_binary(Method),
    SpanName = <<"HTTP ", BinMethod/binary>>,
    UserAgent = elli_request:get_header(<<"User-Agent">>, Req, <<>>),
    {PeerIp, PeerPort} = peer_ip_and_port(Req),

    Flavor = flavor(Req),

    ParsedMethod = parse_method(BinMethod),
    Scheme = extract_scheme(Req),

    Headers = elli_request:headers(Req),

    ClientHeaders = filter_headers(Headers, [<<"forwarded">>, <<"x-forwarded-for">>]),
    ClientInfo = try
        otel_http:extract_client_info(ClientHeaders)
    catch
        _:_ ->
            #{ip => PeerIp, port => undefined}
    end,

    ServerHeaders = filter_headers(Headers, [<<"forwarded">>, <<"x-forwarded-host">>, <<":authority">>, <<"host">>]),
    ServerInfo = try
        otel_http:extract_server_info(ServerHeaders)
    catch
        _:_ ->
            #{address => undefined, port => undefined}
    end,

    Path = strip_query_string(RawPath),

    Attrs1 = #{
        ?CLIENT_ADDRESS => maps:get(ip, ClientInfo, PeerIp),
        ?HTTP_REQUEST_METHOD => ParsedMethod,
        ?NETWORK_PEER_ADDRESS => PeerIp,
        ?NETWORK_PEER_PORT => PeerPort,
        ?URL_PATH => Path,
        ?URL_SCHEME => Scheme,
        ?USER_AGENT_ORIGINAL => UserAgent
    },

    Attrs2 = set_network_protocol_attrs(Attrs1, Flavor),
    Attrs3 = set_query_string_attr(Attrs2, RawPath),
    Attrs4 = set_server_address_attrs(Attrs3, ServerInfo),

    SpanCtx = ?start_span(SpanName, #{kind => ?SPAN_KIND_SERVER,
                                      attributes => Attrs4}),

    ?set_current_span(SpanCtx),
    ok.

parse_method(Method) when is_binary(Method) ->
    case Method of
        <<"CONNECT">> -> ?HTTP_REQUEST_METHOD_VALUES_CONNECT;
        <<"DELETE">> -> ?HTTP_REQUEST_METHOD_VALUES_DELETE;
        <<"GET">> -> ?HTTP_REQUEST_METHOD_VALUES_GET;
        <<"HEAD">> -> ?HTTP_REQUEST_METHOD_VALUES_HEAD;
        <<"OPTIONS">> -> ?HTTP_REQUEST_METHOD_VALUES_OPTIONS;
        <<"PATCH">> -> ?HTTP_REQUEST_METHOD_VALUES_PATCH;
        <<"POST">> -> ?HTTP_REQUEST_METHOD_VALUES_POST;
        <<"PUT">> -> ?HTTP_REQUEST_METHOD_VALUES_PUT;
        <<"TRACE">> -> ?HTTP_REQUEST_METHOD_VALUES_TRACE;
        _ -> ?HTTP_REQUEST_METHOD_VALUES_OTHER
    end.

flavor(#req{version={1,1}}) ->
    <<"1.1">>;
flavor(#req{version={1,0}}) ->
    <<"1.0">>;
flavor(_) ->
    <<"">>.

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

extract_scheme(Req) ->
    Headers = elli_request:headers(Req),
    case otel_http:extract_scheme(Headers) of
        undefined ->
            case Req#req.socket of
                {ssl, _} -> https;
                _ -> http
            end;
        Scheme -> Scheme
    end.

strip_query_string(RawPath) ->
    case binary:split(RawPath, <<"?">>) of
        [Path, _] -> Path;
        [Path] -> Path
    end.

filter_headers(Headers, Names) ->
    [{K, V} || {K, V} <- Headers, lists:member(string:lowercase(K), Names)].

set_query_string_attr(Attrs, RawPath) ->
    case binary:split(RawPath, <<"?">>) of
        [_, QueryString] -> maps:put(?URL_QUERY, QueryString, Attrs);
        [_] -> Attrs
    end.

set_network_protocol_attrs(Attrs, Flavor) ->
    case Flavor of
        <<"1.1">> -> maps:put(?NETWORK_PROTOCOL_VERSION, <<"1.1">>, Attrs);
        <<"1.0">> -> maps:put(?NETWORK_PROTOCOL_VERSION, <<"1.0">>, Attrs);
        _ -> Attrs
    end.

set_server_address_attrs(Attrs, ServerInfo) ->
    case ServerInfo of
        #{address := undefined} -> Attrs;
        #{address := Address, port := undefined} ->
            maps:put(?SERVER_ADDRESS, Address, Attrs);
        #{address := Address, port := Port} ->
            maps:merge(Attrs, #{?SERVER_ADDRESS => Address, ?SERVER_PORT => Port})
    end.

peername({plain, Socket}) ->
    inet:peername(Socket);
peername({ssl, Socket}) ->
    ssl:peername(Socket).

peer_ip_and_port(#req{socket=Socket}) ->
    case peername(Socket) of
        {ok, {Address, Port}} ->
            {list_to_binary(inet_parse:ntoa(Address)), Port};
        _ ->
            undefined
    end.
