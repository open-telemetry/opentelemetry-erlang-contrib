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
         otel_meter:create_observable_gauge(
           Meter, 'diameter.message.count',
           #{description => ~"Number of requests.",
             unit => '{request}'}),
         otel_meter:create_observable_gauge(
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
                  gather_service(SvcName, Info, Stats)
          end, #{}, Services),
    [{Metric, [{V, Attrs} || Attrs := V <- Values]} || Metric := Values <- Stats].

gather_service(SvcName, Info, Stats) ->
    lists:foldl(
      fun({applications, Apps}, S0) ->
              Attrs = #{'diameter.service.name' => SvcName},
              add(['diameter.application.count', Attrs], length(Apps), S0);
         ({peers, Peers}, S0) ->
              Apps = proplists:get_value(applications, Info, []),
              lists:foldl(
                fun({PeerName, Peer}, S1) ->
                        gather_peer(SvcName, PeerName, Peer, Apps, S1)
                end, S0, Peers)
      end, Stats, Info).

gather_peer(SvcName, Peer, Info, Apps, Stats) ->
    lists:foldl(
      fun({connections, C}, S0) ->
              lists:foldl(
                fun(X, S1) ->
                        gather_connection(SvcName, Peer, X, S1)
                end, S0, C);
         ({statistics, S}, S0) ->
              gather_statistics(SvcName, Peer, S, Apps, S0)
      end, Stats, Info).

gather_connection(SvcName, Peer, Values, Stats) ->
    {_, _, State} = proplists:get_value(watchdog, Values, {undefine, undefined, unknown}),
    Type = case proplists:get_value(type, Values, unknown) of
               accept  -> responder;
               connect -> initiator;
               Other -> Other
           end,
    Port = proplists:get_value(port, Values, []),
    Connection = case proplists:get_value(module, Port, unknown) of
                     diameter_tcp -> tcp;
                     diameter_sctp -> sctp;
                     OtherTP -> OtherTP
                 end,
    Attrs = #{'diameter.service.name' => SvcName,
              'diameter.peer.origin_host' => Peer,
              'network.transport' => Connection,
              'diameter.connection.watchdog.state' => State,
              'diameter.role' => Type},
    add(['diameter.connection.count', Attrs], 1, Stats).

gather_statistics(SvcName, Peer, S, Apps, Stats) ->
    Attrs0 =
        #{'diameter.service.name' => SvcName,
          'diameter.peer.origin_host' => Peer},
    lists:foldl(
      fun({{{_, _, 1} = Msg, Direction}, Cnt}, S1) ->
              Attrs1 = Attrs0#{'message.direction' => msg_direction(Direction),
                               'diameter.command.type' => msg_type(Msg),
                               'diameter.application.id' => msg_app_id(Msg),
                               'diameter.command.code' => msg_code(Msg)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.message.count', Attrs], Cnt, S1);
         ({{Msg, Direction, {'Result-Code', RC}}, Cnt}, S1) ->
              Attrs1 = Attrs0#{'message.direction' => msg_direction(Direction),
                               'diameter.command.type' => msg_type(Msg),
                               'diameter.application.id' => msg_app_id(Msg),
                               'diameter.command.code' => msg_code(Msg),
                               'diameter.result_code' => integer_to_binary(RC)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.message.count', Attrs], Cnt, S1);
         ({{Msg, Direction, error}, Cnt}, S1) ->
              Attrs1 = Attrs0#{'message.direction' => msg_direction(Direction),
                               'diameter.command.type' => msg_type(Msg),
                               'diameter.application.id' => msg_app_id(Msg),
                               'diameter.command.code' => msg_code(Msg),
                               'diameter.error.type' => ~"unknown"},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.errors.count', Attrs], Cnt, S1);
         ({{Msg, Direction, Result}, Cnt}, S1) when is_atom(Result) ->
              Attrs1 = Attrs0#{'message.direction' => msg_direction(Direction),
                               'diameter.command.type' => msg_type(Msg),
                               'diameter.application.id' => msg_app_id(Msg),
                               'diameter.command.code' => msg_code(Msg),
                               'diameter.error.type' => atom_to_binary(Result)},
              Attrs = msg_name(Msg, Apps, Attrs1),
              add(['diameter.errors.count', Attrs], Cnt, S1);
         ({Result, Cnt}, S1) when is_atom(Result) ->
              add(['diameter.errors.count', Attrs0], Cnt, S1);
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
