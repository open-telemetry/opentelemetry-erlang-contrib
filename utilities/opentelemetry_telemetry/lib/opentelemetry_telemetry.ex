defmodule OpentelemetryTelemetry do
  @moduledoc """
  `OpentelemetryTelemetry` provides conveniences for leveraging `telemetry`
  events for `OpenTelemetry` bridge libraries.

  ## OpenTelemetry Contexts

  `opentelemetry` does not automatically set current span context when ending
  another span. Since `telemetry` events are executed in separate handlers with
  no shared context, correlating individual events requires a mechanism to do so.
  Additionally, when ending telemetry-based spans, the user must set the correct
  parent context back as the current context. This ensures sibling spans are
  correctly correlated to the shared parent span.

  This library provides helper functions to manage contexts automatically with
  `start_telemetry_span/4`, `set_current_telemetry_span/2`, and `end_telemetry_span/2`
  to give bridge library authors a mechanism for working with these challenges. Once
  `start_telemetry_span/4` or `set_current_telemetry_span/2` are called, users
  can use all of `OpenTelemetry` as normal. By providing the application tracer id
  and the event's metadata, the provided span functions will identify and manage
  span contexts automatically.

  ### Example Telemetry Event Handlers

  ```
    def handle_event(_event,
              %{system_time: start_time},
              metadata,
              %{type: :start, tracer_id: tracer_id, span_name: name}) do
      start_opts = %{start_time: start_time}
      OpentelemetryTelemetry.start_telemetry_span(tracer_id, name, metadata, start_opts)
      :ok
    end

    def handle_event(_event,
                %{duration: duration},
                metadata,
                %{type: :stop, tracer_id: tracer_id}) do
        OpentelemetryTelemetry.set_current_telemetry_span(tracer_id, metadata)
        OpenTelemetry.Tracer.set_attribute(:duration, duration)
        OpentelemetryTelemetry.end_telemetry_span(tracer_id, metadata)
        :ok
    end

    def handle_event(_event,
                %{duration: duration},
                %{kind: kind, reason: reason, stacktrace: stacktrace} = metadata,
                %{type: :exception, tracer_id: tracer_id}) do
        ctx = OpentelemetryTelemetry.set_current_telemetry_span(tracer_id, metadata),
        status = Opentelemetry.status(:error, to_string(reason, :utf8))
        OpenTelemetry.Span.record_exception(ctx, kind, reason, stacktrace, [duration: duration])
        OpenTelemetry.Tracer.set_status(status)
        OpentelemetryTelemetry.end_telemetry_span(tracer_id, metadata)
        :ok
      end
    def handle_event(_event, _measurements, _metadata, _config), do: :ok

  ```

  ### Limitations

  Span contexts are currently stored in the process dictionary, so spans can only
  be correlated within a single process at this time. This covers the primary use
  case where library authors have implemented `telemetry:with_span` or the pattern
  established in said function. Non-library authors should use opentelemetry directly
  wherever possible.

  If the `event_metadata` includes a `telemetry_span_context` (introduced in telemetry
  `v0.4.3`), contexts are correlated by the `telemetry_span_context` id to guarantee
  the correct otel span context. Span events in earlier versions of `telemetry` are stored
  in a stack by `tracer_id` to lessen the likelihood of inadvertently closing the wrong
  span.
  """

  @typedoc """
  A span ctx for a telemetry-based span.
  """
  @type telemetry_span_ctx() :: :opentelemetry.span_ctx()

  @typedoc """
  The parent span ctx for a telemetry-based span. This is what the current span ctx was
  at the time of starting a telemetry-based span.
  """
  @type parent_span_ctx() :: :opentelemetry.span_ctx()

  @type ctx_set() :: {parent_span_ctx(), telemetry_span_ctx()}

  @typep tracer_id() :: atom()

  @doc """
  Start a telemetry-based span.
  """
  @spec start_telemetry_span(
          tracer_id(),
          :opentelemetry.span_name(),
          :telemetry.event_metadata(),
          OpenTelemetry.Span.start_opts()
        ) :: OpenTelemetry.span_ctx()
  defdelegate start_telemetry_span(tracer_id, span_name, event_metadata, start_opts),
    to: :otel_telemetry

  @doc """
  Set the current span ctx based on the tracer_id and telemetry event metadata.
  """
  @spec set_current_telemetry_span(tracer_id(), :telemetry.event_metadata()) ::
          OpenTelemetry.span_ctx()
  defdelegate set_current_telemetry_span(tracer_id, event_metadata), to: :otel_telemetry

  @doc """
  End a telemetry-based span based on the `tracer_id` and telemetry event metadata
  and restore the current ctx to the span's parent ctx.
  """
  @spec end_telemetry_span(tracer_id(), :telemetry.event_metadata()) :: :ok
  defdelegate end_telemetry_span(tracer_id, event_metadata), to: :otel_telemetry
end
