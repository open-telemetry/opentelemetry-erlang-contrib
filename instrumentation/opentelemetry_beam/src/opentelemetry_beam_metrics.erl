%% Copyright 2024, Travelping GmbH <info@travelping.com>
%%
%% The documentation and some small code fragments are taken
%% from the prometheus.erl project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>.
%%

-module(opentelemetry_beam_metrics).

-export([setup/0, setup/1]).
-ignore_xref([setup/0, setup/1, setup/2]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Opts) when is_map(Opts) ->
    setup(?current_meter, Opts);
setup(Meter) ->
    setup(Meter, #{}).

setup(Meter, Opts) ->
    system_info_gauge(
      atom_count, Meter, 'beam.atom.count',
      #{description => <<"The number of atom currently existing "
                         "at the local node.">>,
        unit => '{atom}'}),
    system_info_gauge(
      atom_limit, Meter, 'beam.atom.limit',
      #{description => <<"The maximum number of simultaneously existing "
                         "atom at the local node.">>,
        unit => '{atom}'}),

    system_info_gauge(
      dirty_cpu_schedulers, Meter, 'beam.cpu.dirty_cpu_scheduler.count',
      #{description => <<"The number of scheduler dirty CPU scheduler "
                         "threads used by the emulator.">>,
        unit => '{scheduler}'}),
    system_info_gauge(
      dirty_cpu_schedulers_online, Meter, 'beam.cpu.dirty_cpu_scheduler.online',
      #{description => <<"The number of dirty CPU scheduler threads online.">>,
        unit => '{scheduler}'}),
    system_info_gauge(
      dirty_io_schedulers, Meter, 'beam.dirty_io_scheduler.count',
      #{description => <<"The number of scheduler dirty I/O scheduler "
                         "threads used by the emulator.">>,
        unit => '{scheduler}'}),

    case dirty_run_queue_metrics([]) of
        [_, _] ->
            RunQueueInstr =
                [otel_meter:create_observable_gauge(
                   Meter, 'beam.cpu.dirty_cpu_scheduler.run_queue_length',
                   #{description => <<"Length of the dirty CPU run-queue.">>,
                     unit => '{count}'}),
                 otel_meter:create_observable_gauge(
                   Meter, 'beam.cpu.dirty_io_scheduler.run_queue_length',
                   #{description => <<"Length of the dirty IO run-queue.">>,
                     unit => '{count}'})],
            otel_meter:register_callback(Meter, RunQueueInstr, fun dirty_run_queue_metrics/1, []);
        _ ->
            ok
    end,

    system_info_gauge(
      logical_processors, Meter, 'beam.cpu.logical_processors.count',
      #{description => <<"The detected number of logical processors "
                         "configured in the system.">>,
        unit => '{cpu}'}),
    system_info_gauge(
      logical_processors_available, Meter, 'beam.cpu.logical_processors.available',
      #{description => <<"The detected number of logical processors "
                         "available to the Erlang runtime system.">>,
        unit => '{cpu}'}),
    system_info_gauge(
      logical_processors_online, Meter, 'beam.cpu.logical_processors.online',
      #{description => <<"The detected number of logical processors "
                         "online on the system.">>,
        unit => '{cpu}'}),

    system_info_gauge(
      schedulers, Meter, 'beam.cpu.scheduler.count',
      #{description => <<"The number of scheduler threads used by the emulator.">>,
        unit => '{scheduler}'}),
    system_info_gauge(
      schedulers_online, Meter, 'beam.cpu.scheduler.online',
      #{description => <<"The number of schedulers online.">>,
        unit => '{scheduler}'}),

    otel_meter:create_observable_gauge(
      Meter, 'beam.cpu.scheduler.run_queues_length',
      fun(_) -> [{erlang:statistics(run_queue), #{}}] end, [],
      #{description => <<"Length of normal run-queues.">>,
        unit => '{process}'}),

    setup_microstate_metrics(Meter, Opts),

    system_info_gauge(
      thread_pool_size, Meter, 'beam.thread_pool_size',
      #{description => <<"The number of async threads in the async thread pool "
                         "used for asynchronous driver calls.">>,
        unit => '{thread}'}),

    system_info_gauge(
      ets_limit, Meter, 'beam.ets.limit',
      #{description => <<"The maximum number of ETS tables allowed.">>,
        unit => '{table}'}),

    otel_meter:create_observable_gauge(
      Meter, 'beam.ets.count',
      fun(_) -> [{length(ets:all()), #{}}] end, [],
      #{description => <<"Erlang VM ETS Tables count.">>,
        unit => '{table}'}),

    otel_meter:create_observable_gauge(
      Meter, 'beam.dets.count',
      fun(_) -> [{length(dets:all()), #{}}] end, [],
      #{description => <<"Erlang VM DETS Tables count.">>,
        unit => '{table}'}),

    MemAllocInstr =
        [otel_meter:create_observable_gauge(
           Meter, 'beam.memory.allocators.block.count',
           #{description => <<"Count of allocated blocks for the different allocators in the VM.">>,
             unit => '{block}'}),
         otel_meter:create_observable_gauge(
           Meter, 'beam.memory.allocators.block.size',
           #{description => <<"Size of the memory blocks for the different allocators in the VM.">>,
             unit => 'By'}),
         otel_meter:create_observable_gauge(
           Meter, 'beam.memory.allocators.carrier.count',
           #{description => <<"Number of allocated carriers for the different allocators in the VM.">>,
             unit => '{carrier}'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'beam.memory.allocators.carrier.size',
           #{description => <<"Size of the memory carriers for the different allocators in the VM.">>,
             unit => 'By'})],
    otel_meter:register_callback(Meter, MemAllocInstr, fun allocator_metrics/1, []),

    try erlang:memory() of
        _ ->
            MemInstr =
                [
                 otel_meter:create_observable_updowncounter(
                   Meter, 'beam.memory.allocated',
                   #{description => <<"The total amount of memory currently allocated. "
                                      "This is the same as the sum of the memory size "
                                      "for processes and system.">>,
                     unit => 'By'}),
                 otel_meter:create_observable_updowncounter(
                   Meter, 'beam.memory.atoms',
                   #{description => <<"The total amount of memory currently allocated "
                                      "for atoms. This memory is part of the memory "
                                      "presented as system memory.">>,
                     unit => 'By'}),
                 otel_meter:create_observable_updowncounter(
                   Meter, 'beam.memory.processes',
                   #{description => <<"The total amount of memory currently allocated "
                                      "for the Erlang processes.">>,
                     unit => 'By'}),
                 otel_meter:create_observable_updowncounter(
                   Meter, 'beam.memory.system',
                   #{description => <<"The total amount of memory currently allocated "
                                      "for the emulator that is not directly related "
                                      "to any Erlang process. Memory presented as processes "
                                      "is not included in this memory.">>,
                     unit => 'By'})],
            otel_meter:register_callback(Meter, MemInstr, fun vm_memory_gauges/1, [])
    catch _:_ ->
            ok
    end,

    GcInstr =
        [otel_meter:create_observable_counter(
           Meter, 'beam.memory.garbage_collection.bytes_reclaimed',
           #{description => <<"Garbage collection: bytes reclaimed.">>,
             unit => 'By'}),
         otel_meter:create_observable_counter(
           Meter, 'beam.memory.garbage_collection.count',
           #{description => <<"Garbage collection: number of GCs.">>,
             unit => '{count}'}),
         otel_meter:create_observable_counter(
           Meter, 'beam.memory.garbage_collection.words_reclaimed',
           #{description => <<"Garbage collection: words reclaimed.">>,
             unit => '{word}'})],
    otel_meter:register_callback(Meter, GcInstr, fun vm_gc_stats/1, []),

    system_info_gauge(
      port_count, Meter, 'beam.port.count',
      #{description => <<"The number of ports currently existing "
                         "at the local node.">>,
        unit => '{port}'}),
    otel_meter:create_observable_counter(
      Meter, 'beam.port.io',
      fun(_) ->
              {{input, Input}, {output, Output}} = erlang:statistics(io),
              [{Output, #{'port.io.direction' => write}},
               {Input,  #{'port.io.direction' => read}}]
      end, [],
      #{description => <<"Total number of bytes read and written to/from ports.">>,
        unit => 'By'}),
    system_info_gauge(
      port_limit, Meter, 'beam.port.limit',
      #{description => <<"The maximum number of simultaneously existing ports "
                         "at the local node.">>,
        unit => '{port}'}),

    system_info_counter(
      context_switches, Meter, 'beam.process.context_switches',
      #{description => <<"Total number of context switches "
                         "since the system started.">>,
        unit => '{count}'}),
    system_info_gauge(
      process_count, Meter, 'beam.process.count',
      #{description => <<"The number of processes currently existing "
                         "at the local node.">>,
        unit => '{process}'}),

    case Opts of
        #{opt_in := #{'beam.process.cpu.time' := true}} ->
            otel_meter:create_observable_counter(
              Meter, 'beam.process.cpu.time',
              fun(_) ->
                      {Runtime, _} = erlang:statistics(runtime),
                      {WallclockTime, _} = erlang:statistics(wall_clock),

                      [{Runtime / 1.0e3, #{'beam.process.cpu.state' => user}},
                       {WallclockTime / 1.0e3, #{'process.cpu.state' => wall}}]
              end, [],
              #{description => <<"The sum of the runtime for all threads "
                                 "in the Erlang runtime system. "
                                 "Can be greater than wall clock time.">>,
                unit => 's'});
        _ ->
            ok
    end,

    system_info_gauge(
      process_limit, Meter, 'beam.process.limit',
      #{description => <<"The maximum number of simultaneously existing "
                         "processes at the local node.">>,
        unit => '{process}'}),

    otel_meter:create_observable_counter(
      Meter, 'beam.process.reductions',
      fun(_) ->
              {ReductionsTotal, _} = erlang:statistics(reductions),
              [{ReductionsTotal, #{}}]
      end, [],
      #{description => <<"Total reductions.">>,
        unit => '{reductions}'}),

    system_info_gauge(
      wordsize, Meter, 'beam.system.wordsize',
      #{description => <<"The size of Erlang term words in bytes.">>,
        unit => 'By'}),

    ok.

setup_microstate_metrics(Meter, #{opt_in := #{msacc := true}}) ->
    %% this will fail if not all erts_alloc(3) allocators are enabled
    case msacc:available() of
        true ->
            Instruments =
                [otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.async.time',
                   #{description => <<"Async threads are used by various linked-in drivers "
                                      "(mainly the file drivers) do offload non-CPU intensive "
                                      "work. See erl +A for more details.">>,
                     unit => 's'}),
                 otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.aux.time',
                   #{description => <<"Takes care of any work that is not specifically "
                                      "assigned to a scheduler.">>,
                     unit => 's'}),
                 otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.dirty_cpu_scheduler.time',
                   #{description => <<"The threads for long running cpu intensive work. "
                                      "See erl +SDcpu for more details.">>,
                     unit => 's'}),
                 otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.dirty_io_scheduler.time',
                   #{description => <<"The threads for long running I/O work. "
                                      "See erl +SDio for more details.">>,
                     unit => 's'}),
                 otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.poll.time',
                   #{description => <<"Does the IO polling for the emulator. "
                                      "See erl +IOt for more details.">>,
                     unit => 's'}),
                 otel_meter:create_observable_counter(
                   Meter, 'beam.cpu.scheduler.time',
                   #{description => <<"The main execution threads that do most of the work. "
                                      "See erl +S for more details.">>,
                     unit => 's'})
                ],

            otel_meter:register_callback(Meter, Instruments, fun vm_msacc_counters/1, []),
            ok;
        _ ->
            ok
    end;
setup_microstate_metrics(_, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

system_info_metric(Item) ->
    [{erlang:system_info(Item), #{}}].

system_info_counter(Item, Meter, Name, Opts) ->
    try erlang:system_info(Item) of
        Value when is_integer(Value) ->
            otel_meter:create_observable_updowncounter(
              Meter, Name, fun system_info_metric/1, Item, Opts);
        _ -> ok
    catch error:badarg ->
            ok
    end.

system_info_gauge(Item, Meter, Name, Opts) ->
    try erlang:system_info(Item) of
        Value when is_integer(Value) ->
            otel_meter:create_observable_gauge(
              Meter, Name, fun system_info_metric/1, Item, Opts);
        _ -> ok
    catch error:badarg ->
            ok
    end.

dirty_run_queue_metrics(_) ->
    try
        SO = erlang:system_info(schedulers_online),
        RQ = erlang:statistics(run_queue_lengths_all),
        case length(RQ) > SO of
            true ->
                [DirtyCPURunQueueLength, DirtyIORunQueueLength] =
                    lists:sublist(RQ, length(RQ) - 1, 2),
                [{'beam.cpu.dirty_cpu_scheduler.run_queue_length', [{DirtyCPURunQueueLength, #{}}]},
                 {'beam.cpu.dirty_io_scheduler.run_queue_length', [{DirtyIORunQueueLength, #{}}]}];
            false ->
                []
        end
    catch _:_ -> []
    end.

vm_gc_stats(_) ->
    {NumberOfGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    WordSize = erlang:system_info(wordsize),
    [{'beam.memory.garbage_collection.count', [{NumberOfGCs, #{}}]},
     {'beam.memory.garbage_collection.bytes_reclaimed', [{WordsReclaimed * WordSize, #{}}]},
     {'beam.memory.garbage_collection.words_reclaimed', [{WordsReclaimed, #{}}]}
    ].

allocator_metrics(_) ->
    lists:flatten([begin
                       [
                        [
                         allocator_metric(Alloc, Instance, Kind, Key, KindInfo)
                         || Key <- [carriers, carriers_size]] ++
                            [
                             allocator_blocks_metric(Alloc, Instance, Kind, Key, KindInfo)
                             || Key <- [count, size]]

                        || {Kind, KindInfo} <- Info, (Kind =:= mbcs) orelse (Kind =:= mbcs_pool) orelse (Kind =:= sbcs)]
                   end || {{Alloc, Instance}, Info} <- allocators()]).

allocator_metric(Alloc, Instance, Kind, Key, Values) ->
    Value = element(2, lists:keyfind(Key, 1, Values)),
    allocator_metric_value(Alloc, Instance, Kind, Key, Value).

allocator_metric_value(Alloc, Instance, Kind, Usage, Value) ->
    MetricName =
        case Usage of
            carriers -> 'beam.memory.allocators.carrier.count';
            carriers_size -> 'beam.memory.allocators.carrier.size';
            blocks -> 'beam.memory.allocators.block.count';
            blocks_size -> 'beam.memory.allocators.block.size'
        end,
    Attrs = #{'beam.memory.allocators.alloc' => Alloc,
              'beam.memory.allocators.instance_no' => Instance,
              'beam.memory.allocators.kind' => Kind},
    {MetricName, [{Value, Attrs}]}.

%% Originally copied from recon_alloc.
allocators() ->
    Allocators = erlang:system_info(alloc_util_allocators),
    [{{A, N}, Props} ||
        A <- Allocators,
        Allocs <- [erlang:system_info({allocator, A})],
        Allocs =/= false,
        {_, N, Props} <- Allocs].

allocator_blocks_metric(Alloc, Instance, Kind, count, KindInfo) ->
    Count = case lists:keyfind(blocks, 1, KindInfo) of
                {blocks, L} when is_list(L) ->
                    sum_alloc_block_list(count, L, 0);
                Tuple ->
                    element(2, Tuple)
            end,
    allocator_metric_value(Alloc, Instance, Kind, blocks, Count);
allocator_blocks_metric(Alloc, Instance, Kind, size, KindInfo) ->
    Size = case lists:keyfind(blocks_size, 1, KindInfo) of
               false ->
                   sum_alloc_block_list(size, element(2, lists:keyfind(blocks, 1, KindInfo)), 0);
               Tuple ->
                   element(2, Tuple)
           end,
    allocator_metric_value(Alloc, Instance, Kind, blocks_size, Size).

sum_alloc_block_list(Type, [{_, L} | Rest], Acc) ->
    Value = case lists:keyfind(Type, 1, L) of
                false -> 0;
                Tuple -> element(2, Tuple)
            end,
    sum_alloc_block_list(Type, Rest, Value + Acc);
sum_alloc_block_list(Type, [_ | Rest], Acc) ->
    sum_alloc_block_list(Type, Rest, Acc);
sum_alloc_block_list(_Type, [], Acc) ->
    Acc.

-define(gv(Key, PropList), proplists:get_value(Key, PropList)).

memory_other(Data) ->
    ?gv(system, Data) - ?gv(atom, Data) - ?gv(binary, Data) - ?gv(code, Data) - ?gv(ets, Data).

vm_memory_gauges(_) ->
    Data = erlang:memory(),

    [{'beam.memory.atoms',
      [{?gv(atom_used, Data), #{usage => used}},
       {?gv(atom, Data) - ?gv(atom_used, Data), #{usage => free}}]},
     {'beam.memory.allocated',
      [{?gv(system, Data), #{kind => system}},
       {?gv(processes, Data), #{kind => processes}}]},
     {'beam.memory.processes',
      [{?gv(processes_used, Data), #{usage => used}},
       {?gv(processes, Data) - ?gv(processes_used, Data), #{usage => free}}]},
     {'beam.memory.system',
      [{?gv(atom, Data), #{usage => atom}},
       {?gv(binary, Data), #{usage => binary}},
       {?gv(code, Data), #{usage => code}},
       {?gv(ets, Data), #{usage => ets}},
       {memory_other(Data), #{usage => other}}]}].

vm_msacc_counters(_) ->
    SecondAsPerfCounter = erlang:convert_time_unit(1, second, perf_counter),
    Data = erlang:statistics(microstate_accounting),

    lists:foldl(
      fun (#{type := Type, id := Id, counters := Counters}, M0) ->
              MetricName = binary_to_existing_atom(
                             <<"beam.cpu.", (atom_to_binary(Type))/binary, ".time">>),
              Values =
                  maps:fold(
                    fun(Counter, Value, M1) ->
                            Attrs = #{'beam.cpu.logical_number' => Id,
                                      'beam.cpu.work' => Counter},
                            [{Value / SecondAsPerfCounter, Attrs} | M1]
                    end, [], Counters),
              [{MetricName, Values} | M0]
      end, [], Data).
