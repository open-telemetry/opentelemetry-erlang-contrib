-module(opentelemetry_process_propagator).

-export([fetch_ctx/1,
         fetch_parent_ctx/0,
         fetch_parent_ctx/1,
         fetch_parent_ctx/2]).

-spec fetch_parent_ctx() -> otel_ctx:t() | undefined.
fetch_parent_ctx() ->
    fetch_parent_ctx(1, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer()) -> otel_ctx:t() | undefined.
fetch_parent_ctx(MaxDepth) ->
    fetch_parent_ctx(MaxDepth, '$ancestors').

-spec fetch_parent_ctx(non_neg_integer(), atom()) -> otel_ctx:t() | undefined.
fetch_parent_ctx(MaxDepth, Key) ->
    Pids = case get(Key) of
        List when is_list(List) -> List;
        _ -> []
    end,
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

-spec fetch_ctx(pid() | atom()) -> otel_ctx:t() | undefined.
fetch_ctx(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> undefined;
        Pid -> pdict(Pid)
    end;
fetch_ctx(Pid) when is_pid(Pid) ->
    pdict(Pid).

-spec pdict(pid()) -> otel_ctx:t() | undefined.
-if(?OTP_RELEASE >= 27).
%% Fetching a single key from another process's dictionary was introduced in 26.2,
%% so we can't depend on it until 27.
pdict(Pid) when node(Pid) =:= node() ->
    case process_info(Pid, {dictionary, '$__current_otel_ctx'}) of
        undefined -> undefined;
        {{dictionary, '$__current_otel_ctx'}, Ctx} -> Ctx
    end;
pdict(_) ->
    undefined.

-else.
pdict(Pid) when node(Pid) =:= node() ->
    case process_info(Pid, dictionary) of
        undefined -> undefined;
        {dictionary, Dict} -> otel_ctx(Dict)
    end;
pdict(_) ->
    undefined.

-spec otel_ctx([{term(), term()}]) -> otel_ctx:t() | undefined.
otel_ctx(Dictionary) ->
    case lists:keyfind('$__current_otel_ctx', 1, Dictionary) of
        false ->
            undefined;
        {'$__current_otel_ctx', Ctx} ->
            Ctx
    end.
-endif.
