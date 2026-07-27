defmodule OpentelemetryRedix do
  @moduledoc """
  OpentelemetryRedix uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
  create `OpenTelemetry` spans.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryRedix.setup()

        # ...
      end

  ## Options

  * `:db_namespace` - The Redis database index (e.g., `"0"`). Maps to the
    `db.namespace` semantic convention attribute. Omitted if not provided.
  * `:opt_in_attrs` - List of opt-in attributes to enable. Defaults to `[]`.
  * `:opt_out_attrs` - List of recommended attributes to disable. Defaults to `[]`.
  * `:extra_attrs` - User-defined attributes included on all spans. Instrumented
    attributes take precedence. Defaults to `%{}`.
  """

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.DBAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpentelemetryRedix.Command
  alias OpentelemetryRedix.ConnectionTracker
  alias OpentelemetryRedix.RedixAttributes

  require OpenTelemetry.Tracer

  @default_redis_port 6379

  opt_ins = [
    RedixAttributes.redix_connection_name()
  ]

  opt_outs = [
    DBAttributes.db_query_text()
  ]

  @options_schema NimbleOptions.new!(
                    db_namespace: [
                      type: :string,
                      required: false,
                      doc: """
                      The Redis database index (e.g., `"0"`). Maps to the `db.namespace`
                      semantic convention attribute. Omitted if not provided.
                      """
                    ],
                    opt_in_attrs: [
                      type: {:list, {:in, opt_ins}},
                      default: [],
                      type_spec: quote(do: opt_in_attrs()),
                      doc: """
                      List of opt-in attributes to enable.

                      Use the semantic conventions library to ensure compatibility.

                      Opt-In Attributes:
                      #{Enum.map_join(opt_ins, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    opt_out_attrs: [
                      type: {:list, {:in, opt_outs}},
                      default: [],
                      type_spec: quote(do: opt_out_attrs()),
                      doc: """
                      List of recommended attributes to opt out of.

                      Use the semantic conventions library to ensure compatibility.

                      Recommended Attributes:
                      #{Enum.map_join(opt_outs, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    extra_attrs: [
                      type: :map,
                      type_spec: quote(do: OpenTelemetry.attributes_map()),
                      default: %{},
                      doc: """
                      User-defined attributes included on all spans. Instrumented
                      attributes take precedence over anything supplied here.
                      """
                    ]
                  )

  @typedoc "Use semantic conventions library to ensure compatibility, e.g. `RedixAttributes.redix_connection_name()`"
  @type opt_in_attr() :: unquote(RedixAttributes.redix_connection_name())

  @type opt_in_attrs() :: [opt_in_attr()]

  @typedoc "Use semantic conventions library to ensure compatibility, e.g. `DBAttributes.db_query_text()`"
  @type opt_out_attr() :: unquote(DBAttributes.db_query_text())

  @type opt_out_attrs() :: [opt_out_attr()]

  @typedoc "Setup options"
  @type opts :: [unquote(NimbleOptions.option_typespec(@options_schema))]

  @doc """
  Initializes and configures the telemetry handlers.

  Supported options:\n#{NimbleOptions.docs(@options_schema)}
  """
  @spec setup(opts()) :: :ok
  def setup(opts \\ []) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})

    :telemetry.attach(
      {__MODULE__, :pipeline_stop},
      [:redix, :pipeline, :stop],
      &__MODULE__.handle_pipeline_stop/4,
      config
    )
  end

  @doc false
  def handle_pipeline_stop(_event, measurements, meta, config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    {span_name, operation, batch_size} =
      case meta.commands do
        [[op | _args]] ->
          name = to_string(op)
          {name, name, nil}

        commands ->
          ops = Enum.map(commands, fn [op | _] -> to_string(op) end)

          name =
            case Enum.uniq(ops) do
              [cmd] -> "PIPELINE " <> cmd
              _ -> "PIPELINE"
            end

          {name, name, length(commands)}
      end

    connection = ConnectionTracker.get_connection(meta.connection)

    attributes =
      %{
        :"db.system.name" => "redis",
        DBAttributes.db_operation_name() => operation
      }
      |> maybe_put_query_text(meta.commands, config)
      |> maybe_put(DBAttributes.db_namespace(), config[:db_namespace])
      |> maybe_put(DBAttributes.db_operation_batch_size(), batch_size)
      |> Map.merge(server_attributes(connection))
      |> maybe_put_opt_in_attrs(connection, config)
      |> Map.merge(config.extra_attrs, fn _k, instrumented, _extra -> instrumented end)

    parent_context =
      case OpentelemetryProcessPropagator.fetch_ctx(self()) do
        :undefined ->
          OpentelemetryProcessPropagator.fetch_parent_ctx(1, :"$callers")

        ctx ->
          ctx
      end

    parent_token =
      if parent_context != :undefined do
        OpenTelemetry.Ctx.attach(parent_context)
      else
        :undefined
      end

    s =
      OpenTelemetry.Tracer.start_span(span_name, %{
        start_time: start_time,
        kind: :client,
        attributes: attributes
      })

    if meta[:kind] == :error do
      if is_exception(meta.reason) do
        OpenTelemetry.Span.record_exception(s, meta.reason, nil, [])
      end

      OpenTelemetry.Span.set_status(s, OpenTelemetry.status(:error, format_error(meta.reason)))
      OpenTelemetry.Span.set_attributes(s, error_attributes(meta.reason))
    end

    OpenTelemetry.Span.end_span(s)

    if parent_token != :undefined do
      OpenTelemetry.Ctx.detach(parent_token)
    end
  end

  defp maybe_put_query_text(attrs, commands, %{opt_out_attrs: opt_outs}) do
    if DBAttributes.db_query_text() in opt_outs do
      attrs
    else
      Map.put(
        attrs,
        DBAttributes.db_query_text(),
        Enum.map_join(commands, "\n", &Command.sanitize/1)
      )
    end
  end

  defp maybe_put_opt_in_attrs(attrs, connection, %{opt_in_attrs: opt_ins}) do
    if RedixAttributes.redix_connection_name() in opt_ins do
      Map.merge(attrs, redix_attributes(connection))
    else
      attrs
    end
  end

  defp server_attributes(%{address: address}) when is_binary(address) do
    case String.split(address, ":") do
      [host, port_str] ->
        port = String.to_integer(port_str)
        attrs = %{ServerAttributes.server_address() => host}

        if port != @default_redis_port do
          Map.put(attrs, ServerAttributes.server_port(), port)
        else
          attrs
        end

      [host] ->
        %{ServerAttributes.server_address() => host}
    end
  end

  defp server_attributes(_), do: %{}

  defp redix_attributes(%{connection_name: nil}), do: %{}

  defp redix_attributes(%{connection_name: name}),
    do: %{RedixAttributes.redix_connection_name() => name}

  defp redix_attributes(_), do: %{}

  defp error_attributes(%{__exception__: true, message: message}) when is_binary(message) do
    status_code = message |> String.split(" ", parts: 2) |> List.first()

    %{
      RedixAttributes.redix_response_status_code() => status_code,
      ErrorAttributes.error_type() => status_code
    }
  end

  defp error_attributes(reason) do
    %{ErrorAttributes.error_type() => inspect(reason)}
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)
end
