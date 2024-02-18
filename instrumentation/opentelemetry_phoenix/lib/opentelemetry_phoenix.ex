defmodule OpentelemetryPhoenix do
  @options_schema NimbleOptions.new!(
                    endpoint_prefix: [
                      type: {:list, :atom},
                      default: [:phoenix, :endpoint],
                      doc: "The endpoint prefix in your endpoint."
                    ],
                    adapter: [
                      type: {:in, [:cowboy2, :bandit, nil]},
                      default: nil,
                      doc: "The phoenix server adapter being used.",
                      type_doc: ":atom"
                    ],
                    liveview: [
                      type: :boolean,
                      default: true,
                      doc: "Whether LiveView traces will be instrumented."
                    ]
                  )

  @moduledoc """
  OpentelemetryPhoenix uses [telemetry](https://hexdocs.pm/telemetry/) handlers to create `OpenTelemetry` spans.

  Current events which are supported include endpoint start/stop, router start/stop,
  and router exceptions.

  ### Supported options
  #{NimbleOptions.docs(@options_schema)}

  If you are using PlugCowboy as your adapter you can add `:opentelemetry_cowboy` to your project
  and pass the `:adapter` option when calling setup. Setting this option will prevent a new
  span from being started and the existing cowboy span to be continued. This is the recommended
  setup for measuring accurate latencies.

  ## Usage

  In your application start:

      def start(_type, _args) do
        :opentelemetry_cowboy.setup()
        OpentelemetryPhoenix.setup(adapter: :cowboy2)

        children = [
          {Phoenix.PubSub, name: MyApp.PubSub},
          MyAppWeb.Endpoint
        ]

        opts = [strategy: :one_for_one, name: MyStore.Supervisor]
        Supervisor.start_link(children, opts)
      end

  """
  require OpenTelemetry.Tracer
  alias OpenTelemetry.SemanticConventions
  alias OpenTelemetry.Tracer
  alias OpentelemetryPhoenix.Reason

  require SemanticConventions.Trace
  require OpenTelemetry.Tracer

  @tracer_id __MODULE__

  @typedoc "Setup options"
  @type opts :: [endpoint_prefix() | adapter()]

  @typedoc "The endpoint prefix in your endpoint. Defaults to `[:phoenix, :endpoint]`"
  @type endpoint_prefix :: {:endpoint_prefix, [atom()]}

  @typedoc "The phoenix server adapter being used. Optional"
  @type adapter :: {:adapter, :cowboy2 | :bandit | term()}

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts()) :: :ok
  def setup(opts \\ []) do
    opts = NimbleOptions.validate!(opts, @options_schema)

    attach_endpoint_start_handler(opts)
    attach_endpoint_stop_handler(opts)
    attach_router_start_handler()
    attach_router_dispatch_exception_handler()

    if opts[:liveview] do
      attach_liveview_handlers()
    end

    :ok
  end

  @doc false
  def attach_endpoint_start_handler(opts) do
    :telemetry.attach(
      {__MODULE__, :endpoint_start},
      opts[:endpoint_prefix] ++ [:start],
      &__MODULE__.handle_endpoint_start/4,
      %{adapter: opts[:adapter]}
    )
  end

  @doc false
  def attach_endpoint_stop_handler(opts) do
    :telemetry.attach(
      {__MODULE__, :endpoint_stop},
      opts[:endpoint_prefix] ++ [:stop],
      &__MODULE__.handle_endpoint_stop/4,
      %{adapter: opts[:adapter]}
    )
  end

  @doc false
  def attach_router_start_handler do
    :telemetry.attach(
      {__MODULE__, :router_dispatch_start},
      [:phoenix, :router_dispatch, :start],
      &__MODULE__.handle_router_dispatch_start/4,
      %{}
    )
  end

  @doc false
  def attach_router_dispatch_exception_handler do
    :telemetry.attach(
      {__MODULE__, :router_dispatch_exception},
      [:phoenix, :router_dispatch, :exception],
      &__MODULE__.handle_router_dispatch_exception/4,
      %{}
    )
  end

  def attach_liveview_handlers do
    :telemetry.attach_many(
      {__MODULE__, :live_view},
      [
        [:phoenix, :live_view, :mount, :start],
        [:phoenix, :live_view, :mount, :stop],
        [:phoenix, :live_view, :mount, :exception],
        [:phoenix, :live_view, :handle_params, :start],
        [:phoenix, :live_view, :handle_params, :stop],
        [:phoenix, :live_view, :handle_params, :exception],
        [:phoenix, :live_view, :handle_event, :start],
        [:phoenix, :live_view, :handle_event, :stop],
        [:phoenix, :live_view, :handle_event, :exception],
        [:phoenix, :live_component, :handle_event, :start],
        [:phoenix, :live_component, :handle_event, :stop],
        [:phoenix, :live_component, :handle_event, :exception]
      ],
      &__MODULE__.handle_liveview_event/4,
      %{}
    )

    :ok
  end

  @doc false
  def handle_endpoint_start(_event, _measurements, meta, config) do
    Process.put({:otel_phoenix, :adapter}, config.adapter)

    case adapter() do
      :cowboy2 ->
        cowboy2_start()

      :bandit ->
        bandit_start()

      _ ->
        default_start(meta)
    end
  end

  defp cowboy2_start do
    OpentelemetryProcessPropagator.fetch_parent_ctx()
    |> OpenTelemetry.Ctx.attach()
  end

  defp bandit_start() do
    OpentelemetryProcessPropagator.fetch_parent_ctx()
    |> OpenTelemetry.Ctx.attach()
  end

  defp default_start(meta) do
    %{conn: conn} = meta
    :otel_propagator_text_map.extract(conn.req_headers)

    peer_data = Plug.Conn.get_peer_data(conn)

    user_agent = header_value(conn, "user-agent")
    peer_ip = Map.get(peer_data, :address)

    attributes = %{
      SemanticConventions.Trace.http_client_ip() => client_ip(conn),
      SemanticConventions.Trace.http_flavor() => http_flavor(conn.adapter),
      SemanticConventions.Trace.http_method() => conn.method,
      SemanticConventions.Trace.http_scheme() => "#{conn.scheme}",
      SemanticConventions.Trace.http_target() => conn.request_path,
      SemanticConventions.Trace.http_user_agent() => user_agent,
      SemanticConventions.Trace.net_host_name() => conn.host,
      SemanticConventions.Trace.net_sock_host_addr() => to_string(:inet_parse.ntoa(conn.remote_ip)),
      SemanticConventions.Trace.net_host_port() => conn.port,
      SemanticConventions.Trace.net_sock_peer_addr() => to_string(:inet_parse.ntoa(peer_ip)),
      SemanticConventions.Trace.net_peer_port() => peer_data.port,
      SemanticConventions.Trace.net_transport() => :"IP.TCP"
    }

    # start the span with a default name. Route name isn't known until router dispatch
    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, "HTTP #{conn.method}", meta, %{
      kind: :server,
      attributes: attributes
    })
  end

  @doc false
  def handle_endpoint_stop(_event, _measurements, meta, _config) do
    case adapter() do
      :cowboy2 ->
        :ok

      :bandit ->
        :ok

      _ ->
        default_stop(meta)
    end
  end

  defp default_stop(meta) do
    %{conn: conn} = meta

    # ensure the correct span is current and update the status
    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    Tracer.set_attribute(SemanticConventions.Trace.http_status_code(), conn.status)

    if conn.status >= 500 do
      Tracer.set_status(OpenTelemetry.status(:error, ""))
    end

    # end the Phoenix span
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  @doc false
  def handle_router_dispatch_start(_event, _measurements, meta, _config) do
    attributes = %{
      :"phoenix.plug" => meta.plug,
      :"phoenix.action" => meta.plug_opts,
      SemanticConventions.Trace.http_route() => meta.route
    }

    Tracer.update_name("#{meta.route}")
    Tracer.set_attributes(attributes)
  end

  @doc false
  def handle_router_dispatch_exception(
        _event,
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace},
        _config
      ) do
    if OpenTelemetry.Span.is_recording(OpenTelemetry.Tracer.current_span_ctx()) do
      {[reason: reason], attrs} =
        Reason.normalize(reason)
        |> Keyword.split([:reason])

      # try to normalize all errors to Elixir exceptions
      exception = Exception.normalize(kind, reason, stacktrace)

      # record exception and mark the span as errored
      Tracer.record_exception(exception, stacktrace, attrs)

      # do not close the span as endpoint stop will still be called with
      # more info, including the status code, which is nil at this stage
    end
  end

  def handle_liveview_event(
        [:phoenix, _live, :mount, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket} = meta
    %{view: live_view} = socket

    attributes = %{}

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.mount",
      meta,
      %{kind: :server}
    )
    |> OpenTelemetry.Span.set_attributes(attributes)
  end

  def handle_liveview_event(
        [:phoenix, _live, :handle_params, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket} = meta
    %{view: live_view} = socket

    attributes = %{}

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_params",
      meta,
      %{kind: :server}
    )
    |> OpenTelemetry.Span.set_attributes(attributes)
  end

  def handle_liveview_event(
        [:phoenix, _live, :handle_event, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket, event: event, params: _params} = meta
    %{view: live_view} = socket

    attributes = %{}

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_event##{event}",
      meta,
      %{kind: :server}
    )
    |> OpenTelemetry.Span.set_attributes(attributes)
  end

  def handle_liveview_event(
        [:phoenix, _live, _event, :stop],
        _measurements,
        meta,
        _handler_configuration
      ) do
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_liveview_event(
        [:phoenix, _live, _action, :exception],
        _,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = meta,
        _
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    exception = Exception.normalize(kind, reason, stacktrace)

    OpenTelemetry.Span.record_exception(ctx, exception, stacktrace, [])
    OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, ""))
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  defp http_flavor({_adapter_name, meta}) do
    case Map.get(meta, :version) do
      :"HTTP/1.0" -> :"1.0"
      :"HTTP/1.1" -> :"1.1"
      :"HTTP/2.0" -> :"2.0"
      :"HTTP/2" -> :"2.0"
      :SPDY -> :SPDY
      :QUIC -> :QUIC
      nil -> ""
    end
  end

  defp client_ip(%{remote_ip: remote_ip} = conn) do
    case header_value(conn, "x-forwarded-for") do
      "" ->
        remote_ip
        |> :inet_parse.ntoa()
        |> to_string()

      ip_address ->
        ip_address
        |> String.split(",", parts: 2)
        |> List.first()
    end
  end

  defp header_value(conn, header) do
    case Plug.Conn.get_req_header(conn, header) do
      [] ->
        ""

      [value | _] ->
        value
    end
  end

  defp adapter do
    Process.get({:otel_phoenix, :adapter})
  end
end
