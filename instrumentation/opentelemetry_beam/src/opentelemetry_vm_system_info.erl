%% Copyright 2024, OpenTelemetry Authors
%%
%% The documentation and some small code fragments are taken
%% from the prometheus.erl project which is covered by the
%% MIT License and Copyright (c) 2016, Ilya Khaprov <dead.trickster@gmail.com>
%% and from the recon project which is covered by the
%% BSD 3-Clause License and Copyright (c) 2012-2023, Fred Hebert
%%
%% @doc
%% Collects Erlang VM metrics using
%% <a href="http://erlang.org/doc/man/erlang.html#system_info-1">
%%   erlang:system_info/1
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `erlang.vm.dirty_cpu_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler dirty CPU scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang.vm.dirty_cpu_schedulers_online'<br/>
%%     Type: gauge.<br/>
%%     The number of dirty CPU scheduler threads online.
%%   </li>
%%   <li>
%%     `erlang.vm.dirty_io_schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler dirty I/O scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang.vm.ets_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of ETS tables allowed.
%%   </li>
%%   <li>
%%     `erlang.vm.logical_processors'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors configured in the system.
%%   </li>
%%   <li>
%%     `erlang.vm.logical_processors_available'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors
%%     available to the Erlang runtime system.
%%   </li>
%%   <li>
%%     `erlang.vm.logical_processors_online'<br/>
%%     Type: gauge.<br/>
%%     The detected number of logical processors online on the system.
%%   </li>
%%   <li>
%%     `erlang.vm.port_count'<br/>
%%     Type: gauge.<br/>
%%     The number of ports currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.port_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing ports at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.process_count'<br/>
%%     Type: gauge.<br/>
%%     The number of processes currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.process_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing processes
%%     at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.schedulers'<br/>
%%     Type: gauge.<br/>
%%     The number of scheduler threads used by the emulator.
%%   </li>
%%   <li>
%%     `erlang.vm.schedulers_online'<br/>
%%     Type: gauge.<br/>
%%     The number of schedulers online.
%%   </li>
%%   <li>
%%     `erlang.vm.thread_pool_size'<br/>
%%     Type: gauge.<br/>
%%     The number of async threads in the async thread pool
%%     used for asynchronous driver calls.
%%   </li>
%%   <li>
%%     `erlang.vm.wordsize'<br/>
%%     Type: gauge.<br/>
%%     The size of Erlang term words in bytes.
%%   </li>
%%   <li>
%%     `erlang.vm.atom_count'<br/>
%%     Type: gauge.<br/>
%%     The number of atom currently existing at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.atom_limit'<br/>
%%     Type: gauge.<br/>
%%     The maximum number of simultaneously existing atom at the local node.
%%   </li>
%%   <li>
%%     `erlang.vm.allocators'<br/>
%%     Type: gauge.<br/>
%%     Allocated (carriers_size) and used (blocks_size) memory
%%     for the different allocators in the VM. See erts_alloc(3).
%%   </li>
%% </ul>
%%
%% @end
-module(opentelemetry_vm_system_info).

-export([setup/0, setup/1]).

-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    setup(?current_meter).

setup(Meter) ->
    Instruments =
        [otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.dirty_cpu_schedulers',
           #{description => <<"The number of scheduler dirty CPU scheduler "
                              "threads used by the emulator.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.dirty_cpu_schedulers_online',
           #{description => <<"The number of dirty CPU scheduler threads online.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.dirty_io_schedulers',
           #{description => <<"The number of scheduler dirty I/O scheduler "
                              "threads used by the emulator.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.ets_limit',
           #{description => <<"The maximum number of ETS tables allowed.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.logical_processors',
           #{description => <<"The detected number of logical processors "
                              "configured in the system.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.logical_processors_available',
           #{description => <<"The detected number of logical processors "
                              "available to the Erlang runtime system.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.logical_processors_online',
           #{description => <<"The detected number of logical processors "
                              "online on the system.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.port_count',
           #{description => <<"The number of ports currently existing "
                              "at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.port_limit',
           #{description => <<"The maximum number of simultaneously existing ports "
                              "at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.process_count',
           #{description => <<"The number of processes currently existing "
                              "at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.process_limit',
           #{description => <<"The maximum number of simultaneously existing "
                              "processes at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.schedulers',
           #{description => <<"The number of scheduler threads used by the emulator.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.schedulers_online',
           #{description => <<"The number of schedulers online.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.thread_pool_size',
           #{description => <<"The number of async threads in the async thread pool "
                              "used for asynchronous driver calls.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.wordsize',
           #{description => <<"The size of Erlang term words in bytes.">>,
             unit => <<"By">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.atom_count',
           #{description => <<"The number of atom currently existing "
                              "at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.atom_limit',
           #{description => <<"The maximum number of simultaneously existing "
                              "atom at the local node.">>}),
         otel_meter:create_observable_gauge(
           Meter, 'erlang.vm.allocators',
           #{description => <<"Allocated (carriers_size) and used (blocks_size) "
                              "memory for the different allocators in the VM. "
                              "See erts_alloc(3).">>})],

    otel_meter:register_callback(Meter, Instruments, fun vm_info/1, []),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_metric(Name) ->
    try
        case erlang:system_info(Name) of
            unknown -> undefined;
            Value -> Value
        end
    catch
        error:badarg -> undefined
    end.

allocator_metrics() ->
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
    {Value, #{alloc => Alloc, instance_no => Instance, kind => Kind, usage => Usage}}.

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

vm_info(_) ->
    [{'erlang.vm.dirty_cpu_schedulers', [{get_metric(dirty_cpu_schedulers), #{}}]},
     {'erlang.vm.dirty_cpu_schedulers_online', [{get_metric(dirty_cpu_schedulers_online), #{}}]},
     {'erlang.vm.dirty_io_schedulers', [{get_metric(dirty_io_schedulers), #{}}]},
     {'erlang.vm.ets_limit', [{get_metric(ets_limit), #{}}]},
     {'erlang.vm.logical_processors', [{get_metric(logical_processors), #{}}]},
     {'erlang.vm.logical_processors_available', [{get_metric(logical_processors_available), #{}}]},
     {'erlang.vm.logical_processors_online', [{get_metric(logical_processors_online), #{}}]},
     {'erlang.vm.port_count', [{get_metric(port_count), #{}}]},
     {'erlang.vm.port_limit', [{get_metric(port_limit), #{}}]},
     {'erlang.vm.process_count', [{get_metric(process_count), #{}}]},
     {'erlang.vm.process_limit', [{get_metric(process_limit), #{}}]},
     {'erlang.vm.schedulers', [{get_metric(schedulers), #{}}]},
     {'erlang.vm.schedulers_online', [{get_metric(schedulers_online), #{}}]},
     {'erlang.vm.thread_pool_size', [{get_metric(thread_pool_size), #{}}]},
     {'erlang.vm.wordsize', [{erlang:system_info(wordsize), #{}}]},
     {'erlang.vm.atom_count', [{get_metric(atom_count), #{}}]},
     {'erlang.vm.atom_limit', [{get_metric(atom_limit), #{}}]},
     {'erlang.vm.allocators', allocator_metrics()}].
