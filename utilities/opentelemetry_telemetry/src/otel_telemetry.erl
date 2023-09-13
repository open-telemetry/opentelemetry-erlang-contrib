-module(otel_telemetry).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([
         init/1,
         init/2,
         handle_event/4,
         start_telemetry_span/4,
         set_current_telemetry_span/2,
         end_telemetry_span/2
]).

-type telemetry_span_ctx() :: opentelemetry:span_ctx().
-type parent_span_ctx() :: opentelemetry:span_ctx().

-type ctx_set() :: {parent_span_ctx(), telemetry_span_ctx()}.

-spec init(atom()) -> ok.
init(Application) ->
    init(Application, []).

-spec init(atom(), []) -> ok.
init(_Application, _Opts) ->
    ok.

-spec start_telemetry_span(atom(), opentelemetry:span_name(), telemetry:event_metadata(), otel_span:start_opts()) -> opentelemetry:span_ctx().
start_telemetry_span(TracerId, SpanName, EventMetadata, Opts) ->
    ParentCtx = otel_tracer:current_span_ctx(),
    Tracer = opentelemetry:get_application_tracer(TracerId),
    Ctx = otel_tracer:start_span(Tracer, SpanName, Opts),
    otel_tracer:set_current_span(Ctx),
    _ = store_ctx({ParentCtx, Ctx}, TracerId, EventMetadata),
    Ctx.

-spec set_current_telemetry_span(atom(), telemetry:event_metadata()) -> opentelemetry:span_ctx() | undefined.
set_current_telemetry_span(TracerId, EventMetadata) ->
    case fetch_telemetry_span_ctx(TracerId, EventMetadata) of
        {_ParentCtx, Ctx} ->
            otel_tracer:set_current_span(Ctx),
            Ctx;
        undefined ->
            undefined
    end.

-spec end_telemetry_span(atom(), telemetry:event_metadata()) -> ok.
end_telemetry_span(TracerId, EventMetadata) ->
    Ctx = pop_ctx(TracerId, EventMetadata),
    case Ctx of
        {ParentCtx, SpanCtx} ->
            otel_span:end_span(SpanCtx),
            otel_tracer:set_current_span(ParentCtx),
            ok;
        undefined ->
            ok
    end.

-spec store_ctx(ctx_set(), atom(), telemetry:event_metadata()) -> ok.
store_ctx(SpanCtxSet, TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            push_to_tracer_stack(SpanCtxSet, TracerId);
        TelemetryCtx ->
            erlang:put({otel_telemetry, TelemetryCtx}, SpanCtxSet)
    end,
    ok.

-spec push_to_tracer_stack(ctx_set(), atom()) -> ok.
push_to_tracer_stack(SpanCtxSet, TracerId) ->
    case erlang:get({otel_telemetry, TracerId}) of
        undefined ->
            erlang:put({otel_telemetry, TracerId}, [SpanCtxSet]);
        Stack ->
            erlang:put({otel_telemetry, TracerId}, [SpanCtxSet | Stack])
    end.

-spec fetch_telemetry_span_ctx(atom(), telemetry:event_metadata()) -> ctx_set() | undefined.
fetch_telemetry_span_ctx(TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            peek_from_tracer_stack(TracerId);
        TelemetryCtx ->
            erlang:get({otel_telemetry, TelemetryCtx})
    end.

-spec peek_from_tracer_stack(atom()) -> ctx_set() | undefined.
peek_from_tracer_stack(TracerId) ->
    case erlang:get({otel_telemetry, TracerId}) of
        undefined ->
            undefined;
        [SpanCtxSet | _Rest] ->
            SpanCtxSet;
        [] ->
            ?LOG_DEBUG("`opentelemetry_telemetry` span ctx tracer stack for "
                       "TracerId ~p in Pid ~p is empty.", [TracerId, self()]),
            undefined
    end.

-spec pop_ctx(atom(), telemetry:event_metadata()) -> ctx_set().
pop_ctx(TracerId, EventMetadata) ->
    case maps:get(telemetry_span_context, EventMetadata, undefined) of
        undefined ->
            pop_from_tracer_stack(TracerId);
        TelemetryCtx ->
            erlang:erase({otel_telemetry, TelemetryCtx})
    end.

pop_from_tracer_stack(TracerId) ->
    case erlang:get({otel_telemetry, TracerId}) of
        undefined ->
            undefined;
        [SpanCtxSet | Rest] ->
            erlang:put({otel_telemetry, TracerId}, Rest),
            SpanCtxSet
    end.

handle_event(_Event,
             _Measurements,
             Metadata,
             #{type := start, tracer_id := TracerId, span_name := Name}) ->
    _Ctx = start_telemetry_span(TracerId, Name, Metadata, #{}),
    ok;
handle_event(_Event,
             _Measurements,
             Metadata,
             #{type := stop, tracer_id := TracerId}) ->
    _Ctx = set_current_telemetry_span(TracerId, Metadata),
    end_telemetry_span(TracerId, Metadata),
    ok;
handle_event(_Event,
             _Measurements,
             #{kind := Kind, reason := Reason, stacktrace := Stacktrace} = Metadata,
             #{type := exception, tracer_id := TracerId}) ->
    Ctx = set_current_telemetry_span(TracerId, Metadata),
    Status = opentelemetry:status(?OTEL_STATUS_ERROR, atom_to_binary(Reason, utf8)),
    otel_span:record_exception(Ctx, Kind, Reason, Stacktrace, []),
    otel_span:set_status(Ctx, Status),
    end_telemetry_span(TracerId, Metadata),
    ok;
handle_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.

