%% Copyright 2024, Travelping GmbH <info@travelping.com>
%%
%% C/C++ code was taken from the prometheus_process_collector project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>.
%%

-module(opentelemetry_process_metrics).

-on_load(init/0).

-export([setup/0, setup/1]).
-ignore_xref([setup/0, setup/1]).

-export([process_metrics/1]).
-nifs([get_process_info/0]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

-define(APPNAME, opentelemetry_process).
-define(LIBNAME, opentelemetry_process).

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    Instr =
        [
         %% OTel process metrics defined by semantic conventions

         otel_meter:create_observable_counter(
           Meter, 'beam.process.cpu.time',
           #{description => <<"Total CPU seconds broken down by different states.">>,
             unit => 's'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'process.memory.usage',
           #{description => <<"The amount of physical memory in use.">>,
             unit => 'By'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'process.memory.virtual',
           #{description => <<"The amount of committed virtual memory.">>,
             unit => 'By'}),
         otel_meter:create_observable_counter(
           Meter, 'process.disk.io',
           #{description => <<"Disk bytes transferred.">>,
             unit => 'By'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'process.thread.count',
           #{description => <<"Process threads count.">>,
             unit => '{thread}'}),
         otel_meter:create_observable_updowncounter(
           Meter, 'process.open_file_descriptor.count',
           #{description => <<"Number of file descriptors in use by the process.">>,
             unit => '{count}'}),
         otel_meter:create_observable_counter(
           Meter, 'process.context_switches',
           #{description => <<"Number of times the process has been context switched.">>,
             unit => '{count}'}),
         otel_meter:create_observable_counter(
           Meter, 'process.paging.faults',
          #{description => <<"Number of times the process has been context switched.">>,
             unit => '{fault}'}),
         otel_meter:create_observable_gauge(
           Meter, 'process.uptime',
           #{description => <<"The time the process has been running.">>,
             unit => 's'}),

         %% Extended process metrics

         otel_meter:create_observable_gauge(
           Meter, 'process.open_file_descriptor.limit',
           #{description => <<"Maximum number of open file descriptors.">>,
             unit => '{count}'}),
         otel_meter:create_observable_gauge(
           Meter, 'process.start_time',
           #{description => <<"Start time of the process since unix epoch in seconds.">>,
             unit => 's'}),
         otel_meter:create_observable_gauge(
           Meter, 'process.memory.usage.max',
           #{description => <<"Maximum resident set size used.">>,
             unit => 'By'}),
         otel_meter:create_observable_counter(
           Meter, 'process.swap.count',
           #{description => <<"Number of times a process was \"swapped\" out of main memory.">>,
             unit => '{count}'}),
         otel_meter:create_observable_counter(
           Meter, 'process.signal.delivered.count',
           #{description => <<"Number of signals delivered.">>,
             unit => '{count}'})
        ],
    otel_meter:register_callback(Meter, Instr, fun process_metrics/1, []),
    ok.

%%%===================================================================
%%% NIF stubs
%%%===================================================================

init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

get_process_info() -> ?nif_stub.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(gv(Key, PropList), proplists:get_value(Key, PropList)).

process_metrics(_) ->
    Data = get_process_info(),

    [
     {'process.cpu.time',
      [{?gv(process_utime_seconds, Data), #{'cpu.mode' => user}},
       {?gv(process_stime_seconds, Data), #{'cpu.mode' => system}}]},
     {'process.memory.usage',
      [{?gv(process_resident_memory_bytes, Data), #{}}]},
     {'process.memory.virtual',
      [{?gv(process_virtual_memory_bytes, Data), #{}}]},
     {'process.disk.io',
      [{?gv(process_disk_reads_total, Data), #{'disk.io.direction' => 'read'}},
       {?gv(process_disk_writes_total, Data), #{'disk.io.direction' => 'write'}}]},
     {'process.thread.count',
      [{?gv(process_threads_total, Data), #{}}]},
     {'process.open_file_descriptor.count',
      [{?gv(process_open_fds, Data), #{}}]},
     {'process.context_switches',
      [{?gv(process_voluntary_context_switches_total, Data),
        #{'process.context_switch_type' => 'voluntary'}},
       {?gv(process_involuntary_context_switches_total, Data),
        #{'process.context_switch_type' => 'involuntary'}}]},
     {'process.paging.faults',
      [{?gv(process_noio_pagefaults_total, Data), #{'process.paging.fault_type' => 'minor'}},
       {?gv(process_io_pagefaults_total, Data), #{'process.paging.fault_type' => 'major'}}]},
     {'process.uptime',
      [{?gv(process_uptime_seconds, Data), #{}}]},

     {'process.open_file_descriptor.limit',
      [{?gv(process_max_fds, Data), #{}}]},
     {'process.start_time',
      [{?gv(process_start_time_seconds, Data), #{}}]},
     {'process.memory.usage.max',
      [{?gv(process_max_resident_memory_bytes, Data), #{}}]},
     {'process.swap.count',
      [{?gv(process_swaps_total, Data), #{}}]},
     {'process.signal.delivered.count',
      [{?gv(process_signals_delivered_total, Data), #{}}]}
    ].
