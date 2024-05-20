defmodule OpentelemetryCase do
  use ExUnit.CaseTemplate

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span

  using do
    quote do
      require Record

      for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
        Record.defrecord(name, spec)
      end

      for {name, spec} <-
            Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
        Record.defrecord(name, spec)
      end
    end
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)

    :ok
  end
end
