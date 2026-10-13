%% Copyright 2026, Travelping GmbH <info@travelping.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% based on our previous work in https://github.com/travelping/prometheus_diameter_collector

-module(opentelemetry_diameter_metrics).

-export([setup/0, setup/1]).
-ignore_xref([setup/0, setup/1, setup/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    Meters =
        [otel_meter:create_observable_updowncounter(
           Meter, 'diameter.application.count',
           #{description => ~"Number of installed DIAMETER applications.",
             unit => '{application}'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'diameter.connection.count',
           #{description => ~"Number of connections to peers.",
             unit => '{connection}'}),
         otel_meter:create_observable_counter(
           Meter, 'diameter.message.count',
           #{description => ~"Number of requests.",
             unit => '{request}'}),
         otel_meter:create_observable_counter(
           Meter, 'diameter.connection.io',
           #{description => ~"Bytes sent/received over a connection.",
             unit => 'By'}),
         otel_meter:create_observable_counter(
           Meter, 'diameter.connection.packets',
           #{description => ~"Packets sent/received over a connection.",
             unit => '{packet}'}),
         otel_meter:create_observable_counter(
           Meter, 'diameter.error.count',
           #{description => ~"Number of errors.",
             unit => '{error}'})
        ],
    otel_meter:register_callback(Meter, Meters, fun diameter_metrics/1, []),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================


add([Key], Value, Stats) ->
    maps:update_with(Key, fun(X) -> X + Value end, Value, Stats);
add([Key|Next], Value, Stats) ->
    maps:update_with(Key, fun(S) -> add(Next, Value, S) end, add(Next, Value, #{}), Stats);
add(Key, Value, Stats) ->
    maps:update_with(Key, fun(X) -> X + Value end, Value, Stats).

diameter_metrics(_) ->
    Services = diameter:services(),
    Stats =
        lists:foldl(
          fun(SvcName, Stats) ->
                  Info = diameter:service_info(SvcName, [applications, peers]),
                  SvcAttrs = #{'diameter.service.name' => SvcName},
                  gather_service(SvcAttrs, Info, Stats)
          end, #{}, Services),
    [{Metric, [{V, Attrs} || Attrs := V <- Values]} || Metric := Values <- Stats].

gather_service(SvcAttrs, Info, Stats) ->
    lists:foldl(
      fun({applications, Apps}, S0) ->
              add(['diameter.application.count', SvcAttrs], length(Apps), S0);
         ({peers, Peers}, S0) ->
              Apps = proplists:get_value(applications, Info, []),
              lists:foldl(
                fun({PeerName, PeerInfo}, S1) ->
                        SvcPeerAttrs = SvcAttrs#{'diameter.peer.origin_host' => PeerName},
                        gather_peer(SvcPeerAttrs, PeerInfo, Apps, S1)
                end, S0, Peers)
      end, Stats, Info).

gather_peer(SvcPeerAttrs, Info, Apps, Stats) ->
    lists:foldl(
      fun({connections, C}, S0) ->
              lists:foldl(
                fun(X, S1) ->
                        gather_connection(SvcPeerAttrs, X, S1)
                end, S0, C);
         ({statistics, S}, S0) ->
              gather_statistics(SvcPeerAttrs, S, Apps, S0)
      end, Stats, Info).

gather_connection(SvcPeerAttrs, Values, Stats0) ->
    {_, _, State} = proplists:get_value(watchdog, Values, {undefine, undefined, unknown}),
    Type = case proplists:get_value(type, Values, unknown) of
               accept  -> responder;
               connect -> initiator;
               Other -> Other
           end,
    PortInfo = proplists:get_value(port, Values, []), % This is {module, Mod}
    Connection = case proplists:get_value(module, PortInfo, unknown) of
                     diameter_tcp -> tcp;
                     diameter_sctp -> sctp;
                     OtherTP -> OtherTP
                 end,
    Attrs0 =
        case Connection of
            tcp ->
                try
                    {LocalIP, LocalPort} = proplists:get_value(socket, PortInfo),
                    {PeerIP, PeerPort} = proplists:get_value(peer, PortInfo),
                    SvcPeerAttrs#{'network.local.address' => inet:ntoa(LocalIP),
                                  'network.local.port' => LocalPort,
                                  'network.peer.address' => inet:ntoa(PeerIP),
                                  'network.peer.port' => PeerPort}
                catch
                    C:E:St ->
                        ?LOG(debug, "unexpected failure in TCP socket/peer info, C: ~0p, E: ~0, St: ~0p", [C, E, St]),
                        ok
                end;
            sctp ->
                %% sctp seem broken at least in OTP-28.1
                %%  * socket contains a list of IPs, which is fine for SCTP, but it also
                %%    contains IPs that can be never used on that connection, like 127.0.0.1
                %%  * peer is missing completely
                %% ... and how would a list of IPs match to the attributes anyhow?
                SvcPeerAttrs;
            _ ->
                SvcPeerAttrs
        end,

    OriginRealm = proplists:get_value(origin_realm, proplists:get_value(caps, Values, [])),
    Attrs1 = case OriginRealm of
                 {_OR, DR} -> Attrs0#{'diameter.peer.origin_realm' => DR};
                 _         -> Attrs0
            end,
    Attrs =
        Attrs1#{'network.transport' => Connection,
                'diameter.connection.watchdog.state' => State,
                'diameter.role' => Type},
    Stats = add(['diameter.connection.count', Attrs], 1, Stats0),

    %% Extract network statistics
    lists:foldl(
      fun({recv_cnt, V}, Acc) ->
              %% Number of packets received by the socket.
              add(['diameter.connection.packets', Attrs#{'network.io.direction' => ~"receive"}], V, Acc);
         ({recv_oct, V}, Acc) ->
              %% Number of bytes received by the socket.
              add(['diameter.connection.io', Attrs#{'network.io.direction' => ~"receive"}], V, Acc);
         ({send_cnt, V}, Acc) ->
              %% Number of packets transmit from the socket.
              add(['diameter.connection.packets', Attrs#{'network.io.direction' => ~"transmit"}], V, Acc);
         ({send_oct, V}, Acc) ->
              %% Number of bytes transmit from the socket.
              add(['diameter.connection.io', Attrs#{'network.io.direction' => ~"transmit"}], V, Acc);
         (_, Acc) ->
              Acc
      end, Stats, proplists:get_value(statistics, PortInfo, [])).

gather_statistics(SvcPeerAttrs, S, Apps, Stats) ->
    lists:foldl(
      fun({{{_, _, 1} = Msg, Direction}, Cnt}, S1) ->
              Attrs1 =
                  SvcPeerAttrs#{'message.direction' => msg_direction(Direction),
                                'diameter.command.type' => msg_type(Msg),
                                'diameter.application.id' => msg_app_id(Msg),
                                'diameter.command.code' => msg_code(Msg)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.message.count', Attrs], Cnt, S1);
         ({{Msg, Direction, {'Result-Code', RC}}, Cnt}, S1) ->
              Attrs1 =
                  SvcPeerAttrs#{'message.direction' => msg_direction(Direction),
                                'diameter.command.type' => msg_type(Msg),
                                'diameter.application.id' => msg_app_id(Msg),
                                'diameter.command.code' => msg_code(Msg),
                                'diameter.result_code' => integer_to_binary(RC)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.message.count', Attrs], Cnt, S1);
         ({{Msg, Direction, error}, Cnt}, S1) ->
              Attrs1 =
                  SvcPeerAttrs#{'message.direction' => msg_direction(Direction),
                                'diameter.command.type' => msg_type(Msg),
                                'diameter.application.id' => msg_app_id(Msg),
                                'diameter.command.code' => msg_code(Msg),
                                'diameter.error.type' => ~"unknown"},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.error.count', Attrs], Cnt, S1);
         ({{Msg, Direction, Result}, Cnt}, S1) when is_atom(Result) ->
              Attrs1 =
                  SvcPeerAttrs#{'message.direction' => msg_direction(Direction),
                                'diameter.command.type' => msg_type(Msg),
                                'diameter.application.id' => msg_app_id(Msg),
                                'diameter.command.code' => msg_code(Msg),
                                'diameter.error.type' => atom_to_binary(Result)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.error.count', Attrs], Cnt, S1);
         ({Result, Cnt}, S1) when is_atom(Result) ->
              add(['diameter.error.count', SvcPeerAttrs], Cnt, S1);
         (_, S1) ->
              S1
      end, Stats, S).

msg_direction(recv) -> ~"received";
msg_direction(send) -> ~"sent".

msg_type({_, 0}) -> ~"answer";
msg_type({_, 1}) -> ~"request";
msg_type({_, _, 0}) -> ~"answer";
msg_type({_, _, 1}) -> ~"request".

msg_code({_, Code}) -> integer_to_binary(Code);
msg_code({_, Code, _}) -> integer_to_binary(Code).

msg_app_id({AppId, _}) -> integer_to_binary(AppId);
msg_app_id({AppId, _, _}) -> integer_to_binary(AppId).

try_dict(_Dict, _Cmd, Attrs)
  when is_map_key('diameter.command.name', Attrs) ->
    Attrs;
try_dict(Dict, {_, CmdCode, Rbit} = Cmd, Attrs) ->
    case code:is_loaded(Dict) of
        {file, _} ->
            try Dict:msg_name(CmdCode, Rbit =:= 1) of
                '' -> Cmd;
                Name when is_atom(Name) ->
                    Attrs#{'diameter.command.name' => atom_to_binary(Name)};
                _ ->
                    Attrs
            catch
                _:_ ->
                    Attrs
            end;
        _ ->
            Attrs
    end.

msg_name({0, _, _} = Cmd, _Apps, Attrs0) ->
    Attrs = try_dict(diameter_gen_base_rfc6733, Cmd, Attrs0),
    try_dict(diameter_gen_base_rfc3588, Cmd, Attrs);
msg_name({ApplId, _, _} = Cmd, Apps, Attrs) ->
    case lists:search(fun(E) -> ApplId =:= proplists:get_value(id, E, -1) end, Apps) of
        {value, App} ->
            try_dict(proplists:get_value(dictionary, App, undefined), Cmd, Attrs);
        false ->
            Attrs
    end;
msg_name(_, _Apps, Attrs) ->
    Attrs.
