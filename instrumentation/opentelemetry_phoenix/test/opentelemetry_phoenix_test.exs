defmodule OpentelemetryPhoenixTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryPhoenix

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpentelemetryPhoenix.TestSupport.LiveViewMeta

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    on_exit(fn ->
      Enum.each(:telemetry.list_handlers([]), &:telemetry.detach(&1.id))
    end)

    :ok
  end

  test "omits the route when the socket has no router" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    assert %{} == mount_attributes(LiveViewMeta.put_router(LiveViewMeta.mount_start(), nil))
  end

  test "omits the route when the path matches no route" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    meta = LiveViewMeta.put_uri(LiveViewMeta.mount_start(), "http://localhost:4000/nope")

    assert %{} == mount_attributes(meta)
  end

  defp mount_attributes(meta) do
    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      meta
    )

    assert_receive {:span, span(attributes: attributes)}

    :otel_attributes.map(attributes)
  end
end
