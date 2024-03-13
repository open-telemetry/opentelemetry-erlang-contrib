%% Copyright 2024, OpenTelemetry Authors
%%
%% The documentation and some small code fragments are taken
%% from the prometheus.erl project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>.
%%
%% @doc
%% Collects information about memory dynamically allocated
%% by the Erlang emulator using
%% <a href="http://erlang.org/doc/man/erlang.html#memory-0">
%%   erlang:memory/0
%% </a>, also provides basic (D)ETS statistics.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang.vm_memory.atoms{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     Unit: Bytes.<br/>
%%     The total amount of memory currently allocated for atoms.
%%     This memory is part of the memory presented as system memory.
%%   </li>
%%   <li>
%%     `erlang.vm_memory.allocated{kind="system|processes"}'<br/>
%%     Type: gauge.<br/>
%%     Unit: Bytes.<br/>
%%     The total amount of memory currently allocated.
%%     This is the same as the sum of the memory size for processes and system.
%%   </li>
%%   <li>
%%     `erlang.vm_memory.dets.tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM DETS Tables count.
%%   </li>
%%   <li>
%%     `erlang.vm_memory.ets.tables'<br/>
%%     Type: gauge.<br/>
%%     Erlang VM ETS Tables count.
%%   </li>
%%   <li>
%%     `erlang.vm_memory.processes{usage="free|used"}'<br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the Erlang processes.
%%   </li>
%%   <li>
%%     `erlang.vm_memory.system{usage="atom|binary|code|ets|other"}'
%%     <br/>
%%     Type: gauge.<br/>
%%     The total amount of memory currently allocated for the emulator
%%     that is not directly related to any Erlang process.
%%     Memory presented as processes is not included in this memory.
%%   </li>
%% </ul>
%%
%% @end
-module(opentelemetry_vm_memory).

-export([setup/0, setup/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    %% this will fail if not all erts_alloc(3) allocators are enabled
    erlang:memory(),

    Instruments =
        [otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.memory.atoms',
           #{description => <<"The total amount of memory currently allocated "
                              "for atoms. This memory is part of the memory "
                              "presented as system memory.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.memory.allocated',
           #{description => <<"The total amount of memory currently allocated. "
                              "This is the same as the sum of the memory size "
                              "for processes and system.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.memory.processes',
           #{description => <<"The total amount of memory currently allocated "
                              "for the Erlang processes.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.memory.system',
           #{description => <<"The total amount of memory currently allocated "
                              "for the emulator that is not directly related "
                              "to any Erlang process. Memory presented as processes "
                              "is not included in this memory.">>,
             unit => <<"By">>})],
    otel_meter:register_callback(Meter, Instruments, fun vm_memory_gauges/1, []),

    otel_meter:create_observable_gauge(
      Meter, 'erlang.vm.memory.dets.tables',
      fun(_) -> [{length(dets:all()), #{}}] end, [],
      #{description => <<"Erlang VM DETS Tables count.">>}),
    otel_meter:create_observable_gauge(
      Meter, 'erlang.vm.memory.ets.tables',
      fun(_) -> [{length(ets:all()), #{}}] end, [],
      #{description => <<"Erlang VM ETS Tables count.">>}),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(gv(Key, PropList), proplists:get_value(Key, PropList)).

memory_other(Data) ->
    ?gv(system, Data) - ?gv(atom, Data) - ?gv(binary, Data) - ?gv(code, Data) - ?gv(ets, Data).

vm_memory_gauges(_) ->
    Data = erlang:memory(),

    [{'erlang.vm.memory.atoms',
      [{?gv(atom_used, Data), #{usage => used}},
       {?gv(atom, Data) - ?gv(atom_used, Data), #{usage => free}}]},
     {'erlang.vm.memory.allocated',
      [{?gv(system, Data), #{kind => system}},
       {?gv(processes, Data), #{kind => processes}}]},
     {'erlang.vm.memory.processes',
      [{?gv(processes_used, Data), #{usage => used}},
       {?gv(processes, Data) - ?gv(processes_used, Data), #{usage => free}}]},
     {'erlang.vm.memory.system',
      [{?gv(atom, Data), #{usage => atom}},
       {?gv(binary, Data), #{usage => binary}},
       {?gv(code, Data), #{usage => code}},
       {?gv(ets, Data), #{usage => ets}},
       {memory_other(Data), #{usage => other}}]}].
