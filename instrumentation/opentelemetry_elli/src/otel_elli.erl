-module(otel_elli).

-export([start_span/1]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("elli/include/elli.hrl").

start_span(Req) ->
    Method = elli_request:method(Req),
    RawPath = elli_request:raw_path(Req),

    %% TODO: fix in elli. it currently always returns `undefined'
    %% Scheme = elli_request:scheme(Req),

    %% TODO: update elli to keep the whole url
    %% Url = elli_request:url(Req),

    BinMethod = to_binary(Method),
    SpanName = <<"HTTP ", BinMethod/binary>>,
    UserAgent = elli_request:get_header(<<"User-Agent">>, Req, <<>>),
    Host = case elli_request:host(Req) of
               undefined ->
                   elli_request:get_header(<<"Host">>, Req, <<>>);
               H ->
                   H
           end,
    %% TODO: attribute `http.route' should be an option to this function
    {PeerIp, PeerPort} = peer_ip_and_port(Req),
    {HostIp, HostPort} = host_ip_and_port(Req),
    {ok, HostName} = inet:gethostname(),

    Flavor = flavor(Req),

    SpanCtx = ?start_span(SpanName, #{kind => ?SPAN_KIND_SERVER,
                                      attributes => [{<<"http.target">>, RawPath},
                                                     {<<"http.host">>,  Host},
                                                     %% {<<"http.url">>, Url},
                                                     %% {<<"http.scheme">>,  Scheme},
                                                     {<<"http.flavor">>, Flavor},
                                                     {<<"http.user_agent">>, UserAgent},
                                                     {<<"http.method">>, BinMethod},
                                                     {<<"net.peer.ip">>, PeerIp},
                                                     {<<"net.peer.port">>, PeerPort},
                                                     {<<"net.peer.name">>, Host},
                                                     {<<"net.transport">>, <<"IP.TCP">>},
                                                     {<<"net.host.ip">>, HostIp},
                                                     {<<"net.host.port">>, HostPort},
                                                     {<<"net.host.name">>, HostName}
                                                    | optional_attributes(Req)]}),

    ?set_current_span(SpanCtx),
    ok.

flavor(#req{version={1,1}}) ->
    <<"1.1">>;
flavor(#req{version={1,0}}) ->
    <<"1.0">>;
flavor(_) ->
    <<>>.

to_binary(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
to_binary(Method) ->
    Method.

optional_attributes(Req) ->
    lists:filtermap(fun({Attr, Fun}) ->
                            case Fun(Req) of
                                undefined ->
                                    false;
                                Value ->
                                    {true, {Attr, Value}}
                            end
                    end, [{<<"http.client_ip">>, fun client_ip/1},
                          {<<"http.server_name">>, fun server_name/1}]).

client_ip(Req) ->
    case elli_request:get_header(<<"X-Forwarded-For">>, Req, undefined) of
        undefined ->
            undefined;
        Ip ->
            Ip
    end.

server_name(_) ->
    application:get_env(opentelemetry_elli, server_name, undefined).

peername({plain, Socket}) ->
    inet:peername(Socket);
peername({ssl, Socket}) ->
    ssl:peername(Socket).

sockname({plain, Socket}) ->
    inet:sockname(Socket);
sockname({ssl, Socket}) ->
    ssl:sockname(Socket).

peer_ip_and_port(#req{socket=Socket}) ->
    case peername(Socket) of
        {ok, {Address, Port}} ->
            {list_to_binary(inet_parse:ntoa(Address)), Port};
        _ ->
            undefined
    end.

host_ip_and_port(#req{socket=Socket}) ->
    case sockname(Socket) of
        {ok, {Address, Port}} ->
            {list_to_binary(inet_parse:ntoa(Address)), Port};
        _ ->
            undefined
    end.
