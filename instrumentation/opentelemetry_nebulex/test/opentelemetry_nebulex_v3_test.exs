Application.load(:nebulex)

if Application.spec(:nebulex, :vsn) |> to_string() |> String.starts_with?("3.") do
  defmodule OpentelemetryNebulexV3Test do
    use ExUnit.Case, async: false

    doctest OpentelemetryNebulex

    require OpenTelemetry.Tracer
    require OpenTelemetry.Span
    require Record

    for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
      Record.defrecord(name, spec)
    end

    for {name, spec} <-
          Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
      Record.defrecord(name, spec)
    end

    defmodule Local do
      use Nebulex.Cache,
        otp_app: :opentelemetry_nebulex,
        adapter: Nebulex.Adapters.Local
    end

    defmodule Partitioned do
      use Nebulex.Cache,
        otp_app: :opentelemetry_nebulex,
        adapter: Nebulex.Adapters.Partitioned
    end

    defmodule Multilevel do
      use Nebulex.Cache,
        otp_app: :opentelemetry_nebulex,
        adapter: Nebulex.Adapters.Multilevel

      defmodule L1 do
        use Nebulex.Cache,
          otp_app: :opentelemetry_nebulex,
          adapter: Nebulex.Adapters.Local
      end

      defmodule L2 do
        use Nebulex.Cache,
          otp_app: :opentelemetry_nebulex,
          adapter: Nebulex.Adapters.Partitioned
      end
    end

    setup do
      :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

      OpenTelemetry.Tracer.start_span("test")

      on_exit(fn ->
        OpenTelemetry.Tracer.end_span()
      end)
    end

    test "local cache commands" do
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :local])

      start_supervised!(Local)

      # miss
      Local.get(:my_key)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :miss,
               "nebulex.cache": OpentelemetryNebulexV3Test.Local,
               "db.system": :ets,
               "db.operation.name": :fetch,
               "db.query.text": "FETCH :my_key"
             } = :otel_attributes.map(attributes)

      # write
      Local.put(:my_key, 42)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.cache": OpentelemetryNebulexV3Test.Local,
               "db.system": :ets,
               "db.operation.name": :put,
               "db.query.text": "PUT :my_key"
             } = :otel_attributes.map(attributes)

      # hit
      Local.get(:my_key)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :hit,
               "nebulex.cache": OpentelemetryNebulexV3Test.Local,
               "db.system": :ets,
               "db.operation.name": :fetch,
               "db.query.text": "FETCH :my_key"
             } = :otel_attributes.map(attributes)
    end

    test "partitioned cache commands" do
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :partitioned])
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :partitioned, :primary])

      start_supervised!(Partitioned)

      # miss
      Partitioned.get(:my_key)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :miss,
               "db.system": :ets,
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned.Primary
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :miss,
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned
             } = :otel_attributes.map(attributes)

      # write
      Partitioned.put(:my_key, 42)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "db.system": :ets,
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned.Primary
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned
             } = :otel_attributes.map(attributes)

      # hit
      Partitioned.get(:my_key)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :hit,
               "db.system": :ets,
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned.Primary
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :hit,
               "nebulex.cache": OpentelemetryNebulexV3Test.Partitioned
             } = :otel_attributes.map(attributes)
    end

    test "multi-level cache commands" do
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :multilevel])
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :multilevel, :l1])
      OpentelemetryNebulex.setup([:opentelemetry_nebulex_v3_test, :multilevel, :l2])

      start_supervised!(
        {Multilevel,
         [
           levels: [
             {Multilevel.L1, []},
             {Multilevel.L2, []}
           ]
         ]}
      )

      # write
      Multilevel.put(:my_key, 42)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "db.system": :ets,
               "nebulex.cache": OpentelemetryNebulexV3Test.Multilevel.L1
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.cache": OpentelemetryNebulexV3Test.Multilevel.L2
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex put", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.cache": OpentelemetryNebulexV3Test.Multilevel,
               "nebulex.model": :inclusive
             } = :otel_attributes.map(attributes)

      # hit
      Multilevel.get(:my_key)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :hit,
               "db.system": :ets,
               "nebulex.cache": OpentelemetryNebulexV3Test.Multilevel.L1
             } = :otel_attributes.map(attributes)

      assert_receive {:span, span(name: "nebulex fetch", kind: :internal, attributes: attributes)}

      assert %{
               "nebulex.action": :hit,
               "nebulex.cache": OpentelemetryNebulexV3Test.Multilevel,
               "nebulex.model": :inclusive
             } = :otel_attributes.map(attributes)
    end
  end
end
