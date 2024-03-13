%% Copyright 2024, OpenTelemetry Authors
%%
%% The documentation and some small code fragments are taken
%% from the prometheus.erl project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>.
%%
%% @doc
%% Collects microstate accounting metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#statistics_microstate_accounting">
%%   erlang:statistics(microstate_accounting)
%% </a>.
%%
%% In order for values to increase, microstate
%% accounting must be enabled. This is done by
%% calling <code>erlang:system_flag(microstate_accounting, true).</code>
%%
%% ==Exported metrics==
%% Some metrics are not available by default. They require a VM
%% configured with <code>./configure --with-microstate-accounting=extra</code>.
%%
%% <ul>
%%   <li>
%%     `erlang.vm.msacc.aux'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent handling auxiliary jobs.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.check_io'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent checking for new I/O events.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.emulator'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing Erlang processes.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.gc'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing garbage collection.
%%     When extra states are enabled this is the time spent
%%     doing non-fullsweep garbage collections.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.other'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing unaccounted things.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.port'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing ports.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.sleep'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent sleeping.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.alloc'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent managing memory.
%%     Without extra states this time is spread out over all other states.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.bif'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent in BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.busy_wait'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent busy waiting.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.ets'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent executing ETS BIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.gc_full'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent doing fullsweep garbage collection.
%%     Without extra states this time is part of the 'gc' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.nif'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent in NIFs.
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.send'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent sending messages (processes only).
%%     Without extra states this time is part of the 'emulator' state.
%%   </li>
%%   <li>
%%     `erlang.vm.msacc.timers'<br/>
%%     Type: counter.<br/>
%%     Total time in seconds spent managing timers.
%%     Without extra states this time is part of the 'other' state.
%%   </li>
%% </ul>
%%
%% @end
-module(opentelemetry_vm_msacc).

-export([setup/0, setup/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    %% this will fail if not all erts_alloc(3) allocators are enabled
    [_|_] = erlang:statistics(microstate_accounting),

    Instruments =
        [%% Base states.
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.aux',
           #{description => <<"Total time in seconds spent handling auxiliary jobs.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.check_io',
           #{description => <<"Total time in seconds spent checking for new I/O events.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.emulator',
           #{description => <<"Total time in seconds spent executing Erlang processes.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.gc',
           #{description => <<"Total time in seconds spent doing garbage collection. "
                              "When extra states are enabled this is the time spent "
                              "doing non-fullsweep garbage collections.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.other',
           #{description => <<"Total time in seconds spent doing unaccounted things.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.port',
           #{description => <<"Total time in seconds spent executing ports.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.sleep',
           #{description => <<"Total time in seconds spent sleeping.">>,
             unit => <<"s">>}),
         %% Extra states.
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.alloc',
           #{description =>
                 <<"Total time in seconds spent managing memory. "
                   "Without extra states this time is spread out over all other states.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.bif',
           #{description =>
                 <<"Total time in seconds spent in BIFs. "
                   "Without extra states this time is part of the 'emulator' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.busy_wait',
           #{description =>
                 <<"Total time in seconds spent busy waiting. "
                   "Without extra states this time is part of the 'other' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.ets',
           #{description =>
                 <<"Total time in seconds spent executing ETS BIFs. "
                   "Without extra states this time is part of the 'emulator' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.gc_full',
           #{description =>
                 <<"Total time in seconds spent doing fullsweep garbage collection. "
                   "Without extra states this time is part of the 'gc' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.nif',
           #{description =>
                 <<"Total time in seconds spent in NIFs. "
                   "Without extra states this time is part of the 'emulator' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.send',
           #{description =>
                 <<"Total time in seconds spent sending messages (processes only). "
                   "Without extra states this time is part of the 'emulator' state.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.msacc.timers',
           #{description =>
                 <<"Total time in seconds spent managing timers. "
                   "Without extra states this time is part of the 'other' state.">>,
             unit => <<"s">>})
        ],

    otel_meter:register_callback(Meter, Instruments, fun vm_msacc_counters/1, []),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(gv(Key, PropList), proplists:get_value(Key, PropList)).

%% metric(Counter, Data, SecondAsPerfCounter) ->
%%   [{[{type, Type}, {id, ID}], Value / SecondAsPerfCounter}
%%    || #{type := Type, id := ID, counters := #{Counter := Value}} <- Data].

vm_msacc_counters(_) ->
    SecondAsPerfCounter = erlang:convert_time_unit(1, second, perf_counter),
    Data = erlang:statistics(microstate_accounting),

    lists:foldl(
      fun (#{type := Type, id := Id, counters := Counters}, M0) ->
              maps:fold(
                fun(Counter, Value, M1) ->
                        MetricName = binary_to_existing_atom(<<"erlang.vm.msacc.", (atom_to_binary(Counter))/binary >>),
                        [{MetricName, [{Value / SecondAsPerfCounter, #{type => Type, id => Id}}]} | M1]
                end, M0, Counters)
      end, [], Data).
