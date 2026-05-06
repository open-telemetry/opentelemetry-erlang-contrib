defmodule OpentelemetryCommanded.CommandedCase do
  @moduledoc """
  A case template for tests relying on the CommandedApp
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import OpentelemetryCommanded.CommandedCase
    end
  end

  alias Commanded.Helpers.CommandAuditMiddleware
  alias OpentelemetryCommanded.DummyApp.App

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    start_supervised!(CommandAuditMiddleware)
    start_supervised!(App)
    {:ok, _handler} = OpentelemetryCommanded.DummyApp.EventHandler.start_link()
    {:ok, _pid} = OpentelemetryCommanded.DummyApp.ProcessManager.start_link(start_from: :current)

    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :application.start(:opentelemetry)

    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())

    %{correlation_id: "b802ced4-02de-4f12-943e-42cef58658ed"}
  end

  @tracer_id __MODULE__

  setup do
    :telemetry.attach(
      {__MODULE__, :start},
      [:opentelemetry_commanded, :test, :start],
      &__MODULE__.handle_start/4,
      []
    )

    :telemetry.attach(
      {__MODULE__, :stop},
      [:opentelemetry_commanded, :test, :stop],
      &__MODULE__.handle_stop/4,
      []
    )

    :telemetry.attach(
      {__MODULE__, :exception},
      [:opentelemetry_commanded, :test, :exception],
      &__MODULE__.handle_exception/4,
      []
    )

    :ok
  end

  def app_dispatch(context, command) do
    meta = %{}

    # Add containing span to prove context is properly passed across process boundaries
    :telemetry.span([:opentelemetry_commanded, :test], meta, fn ->
      {
        App.dispatch(command,
          application: OpentelemetryCommanded.DummyApp.App,
          correlation_id: context.correlation_id,
          consistency: :strong
        ),
        meta
      }
    end)
  end

  def handle_start(_event, _, meta, _) do
    attributes = %{
      "test_span.id": inspect(meta.telemetry_span_context, structs: false)
    }

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "opentelemetry_commanded.test",
      meta,
      %{
        kind: :internal,
        attributes: attributes
      }
    )
  end

  def handle_stop(_event, _measurements, meta, _) do
    # ensure the correct span is current
    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_exception(
        _event,
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = meta,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    # try to normalize all errors to Elixir exceptions
    exception = Exception.normalize(kind, reason, stacktrace)

    # record exception and mark the span as errored
    OpenTelemetry.Span.record_exception(ctx, exception, stacktrace)
    OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, inspect(reason)))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end
end
