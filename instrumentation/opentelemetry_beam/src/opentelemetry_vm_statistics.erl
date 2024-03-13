%% Copyright 2024, OpenTelemetry Authors
%%
%% The documentation and some small code fragments are taken
%% from the prometheus.erl project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>.
%%
%% @doc
%% Collects Erlang VM metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#statistics-1">
%%   erlang:statistics/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang.vm.statistics.output'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes output to ports.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.received'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes received through ports.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.context_switches'<br/>
%%     Type: counter.<br/>
%%     The total number of context switches since the system started.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.dirty_cpu_run_queue_length'<br/>
%%     Type: gauge.<br/>
%%     Length of the dirty CPU run-queue.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.dirty_io_run_queue_length'<br/>
%%     Type: gauge.<br/>
%%     Length of the dirty IO run-queue.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.garbage_collection_number_of_gcs'<br/>
%%     Type: counter.<br/>
%%     The total number of garbage collections since the system started.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.garbage_collection_words_reclaimed'<br/>
%%     Type: counter.<br/>
%%     The total number of words reclaimed by GC since the system started.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.garbage_collection_memory_reclaimed'<br/>
%%     Type: counter.<br/>
%%     The total number of bytes reclaimed by GC since the system started.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.reductions'<br/>
%%     Type: counter.<br/>
%%     Total reductions count.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.run_queues_length'<br/>
%%     Type: gauge.<br/>
%%     The total length of all normal run-queues. That is, the number of
%%     processes and ports that are ready to run on all available normal
%%     run-queues.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.runtime'<br/>
%%     Type: counter.<br/>
%%     The sum of the runtime for all threads in the Erlang runtime system.
%%   </li>
%%   <li>
%%     `erlang.vm.statistics.wallclock_time'<br/>
%%     Type: counter.<br/>
%%     Can be used in the same manner as
%%     `erlang.vm.statistics.runtime', except that real time is
%%     measured as opposed to runtime or CPU time.
%%   </li>
%% </ul>
%%
%% @end

-module(opentelemetry_vm_statistics).

-export([setup/0, setup/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    Instruments =
        [otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.output',
           #{description => <<"Total number of bytes output to ports.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.received',
           #{description => <<"Total number of bytes received through ports.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.context_switches',
           #{description => <<"Total number of context switches "
                              "since the system started.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.statistics.dirty_cpu_run_queue_length',
           #{description => <<"Length of the dirty CPU run-queue.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.statistics.dirty_io_run_queue_length',
           #{description => <<"Length of the dirty IO run-queue.">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.reductions',
           #{description => <<"Total reductions.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.statistics.run_queues_length',
           #{description => <<"Length of normal run-queues.">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.runtime',
           #{description => <<"The sum of the runtime for all threads "
                              "in the Erlang runtime system. "
                              "Can be greater than wall clock time.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.wallclock_time',
           #{description => <<"Information about wall clock. "
                              "Same as erlang_vm_statistics_runtime "
                              "except that real time is measured.">>,
             unit => <<"s">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.garbage_collection_number_of_gcs',
           #{description => <<"Garbage collection: number of GCs.">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.garbage_collection_bytes_reclaimed',
           #{description => <<"Garbage collection: bytes reclaimed.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_counter(
           Meter, 'erlang.vm.statistics.garbage_collection_words_reclaimed',
           #{description => <<"Garbage collection: words reclaimed.">>})],

    otel_meter:register_callback(Meter, Instruments, fun vm_stats/1, []),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dirty_stat() ->
    try
        SO = erlang:system_info(schedulers_online),
        RQ = erlang:statistics(run_queue_lengths_all),
        case length(RQ) > SO of
            true -> lists:sublist(RQ, length(RQ) - 1, 2);
            false -> [undefined, undefined]
        end
    catch _:_ -> [undefined, undefined]
    end.

vm_stats(_) ->
    {{input, Input}, {output, Output}} = erlang:statistics(io),
    {ContextSwitches, _} = erlang:statistics(context_switches),
    [DirtyCPURunQueueLength, DirtyIORunQueueLength] = dirty_stat(),
    {NumberOfGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
    WordSize = erlang:system_info(wordsize),
    {ReductionsTotal, _} = erlang:statistics(reductions),
    RunQueuesLength = erlang:statistics(run_queue),
    {Runtime, _} = erlang:statistics(runtime),
    {WallclockTime, _} = erlang:statistics(wall_clock),

    [{'erlang.vm.statistics.output', [{Output, #{}}]},
     {'erlang.vm.statistics.received', [{Input, #{}}]},
     {'erlang.vm.statistics.context_switches', [{ContextSwitches, #{}}]},
     {'erlang.vm.statistics.dirty_cpu_run_queue_length', [{DirtyCPURunQueueLength, #{}}]},
     {'erlang.vm.statistics.dirty_io_run_queue_length', [{DirtyIORunQueueLength, #{}}]},
     {'erlang.vm.statistics.garbage_collection_number_of_gcs', [{NumberOfGCs, #{}}]},
     {'erlang.vm.statistics.garbage_collection_bytes_reclaimed', [{WordsReclaimed * WordSize, #{}}]},
     {'erlang.vm.statistics.garbage_collection_words_reclaimed', [{WordsReclaimed, #{}}]},
     {'erlang.vm.statistics.reductions', [{ReductionsTotal, #{}}]},
     {'erlang.vm.statistics.run_queues_length', [{RunQueuesLength, #{}}]},
     {'erlang.vm.statistics.runtime', [{Runtime / 1.0e3, #{}}]},
     {'erlang.vm.statistics.wallclock_time', [{WallclockTime / 1.0e3, #{}}]}].
