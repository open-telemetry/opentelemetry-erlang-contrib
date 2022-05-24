-module(opentelemetry_process_propagator).

-export([fetch_ctx/1,
         fetch_parent_ctx/0,
         fetch_parent_ctx/1,
         fetch_parent_ctx/2,
         get_ctx/0,
         get_ctx/1,
         get_ctx/2]).

-spec fetch_parent_ctx() -> opentelemetry:span_ctx() | #{}.
fetch_parent_ctx() ->
    fetch_parent_ctx(1, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer()) -> opentelemetry:span_ctx() | #{}.
fetch_parent_ctx(MaxDepth) ->
    fetch_parent_ctx(MaxDepth, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer(), atom()) -> opentelemetry:span_ctx() | #{}.
fetch_parent_ctx(MaxDepth, Key) ->
    Pids = pids(Key, pdict(self())),
    inspect_parent(#{}, lists:sublist(Pids, MaxDepth)).

inspect_parent(Ctx, []) ->
    Ctx;
inspect_parent(Ctx, _Pids) when map_size(Ctx) > 0 ->
    Ctx;
inspect_parent(_Ctx, [Pid | Rest]) ->
    case fetch_ctx(Pid) of
        Ctx when map_size(Ctx) > 0 ->
            Ctx;
        EmptyMap ->
            inspect_parent(EmptyMap, Rest)
    end.

-spec fetch_ctx(pid()) -> opentelemetry:span_ctx() | #{}.
fetch_ctx(Pid) ->
    case pdict(Pid) of
        undefined ->
            #{};
        Dictionary ->
            otel_ctx(Dictionary)
    end.

-spec get_ctx() -> opentelemetry:span_ctx() | #{}.
get_ctx() ->
    case otel_ctx:get_current() of
        Ctx when erlang:map_size(Ctx) == 0 ->
            fetch_parent_ctx();
        Ctx ->
            Ctx
    end.

-spec get_ctx(non_neg_integer()) -> opentelemetry:span_ctx() | #{}.
get_ctx(MaxDepth) ->
    case otel_ctx:get_current() of
        Ctx when map_size(Ctx) == 0 ->
            fetch_parent_ctx(MaxDepth);
        Ctx ->
            Ctx
    end.

-spec get_ctx(non_neg_integer(), atom()) -> opentelemetry:span_ctx() | #{}.
get_ctx(MaxDepth, Key) ->
    case otel_ctx:get_current() of
        Ctx when map_size(Ctx) == 0 ->
            fetch_parent_ctx(MaxDepth, Key);
        Ctx ->
            Ctx
    end.

-spec pdict(pid()) -> [{term(), term()}] | [].
pdict(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            Dict;
        undefined ->
            []
    end.

-spec otel_ctx([{term(), term()}]) -> opentelemetry:span_ctx() | #{}.
otel_ctx(Dictionary) ->
    case lists:keyfind('$__current_otel_ctx', 1, Dictionary) of
        false ->
            #{};
        {'$__current_otel_ctx', Ctx} ->
            Ctx
    end.

pids(Key, Dictionary) ->
    case lists:keyfind(Key, 1, Dictionary) of
        false ->
            [];
        {Key,Pids} ->
            Pids
    end.
