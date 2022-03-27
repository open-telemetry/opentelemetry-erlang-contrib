-module(opentelemetry_process_propagator).

-export([fetch_ctx/1,
         fetch_parent_ctx/0,
         fetch_parent_ctx/1,
         fetch_parent_ctx/2]).

-spec fetch_parent_ctx() -> opentelemetry:span_ctx() | undefined.
fetch_parent_ctx() ->
    fetch_parent_ctx(1, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer()) -> opentelemetry:span_ctx() | undefined.
fetch_parent_ctx(MaxDepth) ->
    fetch_parent_ctx(MaxDepth, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer(), atom()) -> opentelemetry:span_ctx() | undefined.
fetch_parent_ctx(MaxDepth, Key) ->
    Pids = pids(Key, pdict(self())),
    inspect_parent(undefined, lists:sublist(Pids, MaxDepth)).

inspect_parent(Ctx, _Pids) when Ctx =/= undefined ->
    Ctx;
inspect_parent(Ctx, []) ->
    Ctx;
inspect_parent(_Ctx, [Pid | Rest]) ->
    case fetch_ctx(Pid) of
        undefined ->
            inspect_parent(undefined, Rest);
        OtelCtx ->
            inspect_parent(OtelCtx, [])
    end.

-spec fetch_ctx(pid()) -> opentelemetry:span_ctx() | undefined.
fetch_ctx(Pid) ->
    case pdict(Pid) of
        undefined ->
            undefined;
        Dictionary ->
            otel_ctx(Dictionary)
    end.

-spec pdict(pid()) -> [{term(), term()}] | undefined.
pdict(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} ->
            Dict;
        undefined ->
            undefined
    end.

-spec otel_ctx([{term(), term()}]) -> opentelemetry:span_ctx() | undefined.
otel_ctx(Dictionary) ->
    case lists:keyfind('$__current_otel_ctx', 1, Dictionary) of
        false ->
            undefined;
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
