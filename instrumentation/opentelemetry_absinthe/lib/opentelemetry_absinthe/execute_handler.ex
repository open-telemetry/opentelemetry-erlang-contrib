defmodule OpentelemetryAbsinthe.ExecuteHandler do
  @moduledoc false

  alias OpentelemetryAbsinthe.SpanTracker

  @operation_start [:absinthe, :execute, :operation, :start]
  @operation_stop [:absinthe, :execute, :operation, :stop]

  @resolve_field_start [:absinthe, :resolve, :field, :start]
  @resolve_field_stop [:absinthe, :resolve, :field, :stop]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :operation},
      [
        @operation_start,
        @operation_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )

    :telemetry.attach_many(
      {__MODULE__, :resolve_field},
      [
        @resolve_field_start,
        @resolve_field_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@operation_start, _measurements, metadata, config) do
    attributes = [
      "graphql.source": metadata.options[:document]
    ]

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, :"graphql.execute", metadata, %{
      attributes: attributes
    })

    SpanTracker.init()
  end

  def handle_event(@operation_stop, _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    operation = Absinthe.Blueprint.current_operation(metadata.blueprint)

    attributes =
      [
        "absinthe.schema": inspect(metadata.blueprint.schema)
      ]
      |> add_operation_attributes(operation)

    OpenTelemetry.Span.set_attributes(ctx, attributes)

    SpanTracker.terminate()
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@resolve_field_start, _measurements, metadata, config) do
    resolution = metadata.resolution
    path = Absinthe.Resolution.path(resolution)
    type = Absinthe.Type.name(resolution.definition.schema_node.type, resolution.schema)
    parent_type = Absinthe.Type.name(resolution.parent_type)

    span_name = "#{parent_type}.#{resolution.definition.name}"

    attributes =
      [
        "absinthe.schema": inspect(resolution.schema),
        "graphql.field.name": resolution.definition.name,
        "graphql.field.type": type,
        "graphql.field.path": Enum.join(path, ".")
      ]
      |> add_resolver_attributes(resolver_name(resolution))

    parent_ctx = SpanTracker.find_parent_ctx(path)

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, span_name, metadata, %{
      attributes: attributes
    })

    SpanTracker.push_ctx(path)
  end

  def handle_event(@resolve_field_stop, _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    if Enum.any?(metadata.resolution.errors) do
      message = "#{Enum.map_join(metadata.resolution.errors, "\n", &inspect/1)}"
      OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, message))
    end

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  defp add_operation_attributes(attrs, nil), do: attrs

  defp add_operation_attributes(attrs, %{name: nil} = operation) do
    [
      "graphql.operation.type": operation.type
    ] ++ attrs
  end

  defp add_operation_attributes(attrs, operation) do
    [
      "graphql.operation.name": operation.name,
      "graphql.operation.type": operation.type
    ] ++ attrs
  end

  defp add_resolver_attributes(attrs, nil), do: attrs

  defp add_resolver_attributes(attrs, resolver_name) do
    ["absinthe.resolver": resolver_name] ++ attrs
  end

  defp resolver_name(resolution) do
    case resolver_mfa(resolution) do
      {module, function, arity} ->
        "#{inspect(module)}.#{function}/#{arity}"

      nil ->
        nil
    end
  end

  defp resolver_mfa(%{middleware: middleware}) do
    middleware
    |> Enum.find(&match?({{Absinthe.Resolution, :call}, _resolver}, &1))
    |> case do
      {_, {mod, fun}} ->
        {mod, fun, 3}

      {_, fun} when is_function(fun) ->
        info = Function.info(fun)
        {info[:module], info[:name], info[:arity]}

      nil ->
        nil
    end
  end
end
