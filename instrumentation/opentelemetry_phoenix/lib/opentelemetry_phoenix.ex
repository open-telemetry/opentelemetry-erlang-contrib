defmodule OpentelemetryPhoenix do
  @moduledoc """
  OpentelemetryPhoenix uses [telemetry](https://hexdocs.pm/telemetry/) handlers to create `OpenTelemetry` spans.

  Current events which are supported include endpoint start/stop, router start/stop,
  and router exceptions.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryPhoenix.setup()

        children = [
          {Phoenix.PubSub, name: MyApp.PubSub},
          MyAppWeb.Endpoint
        ]

        opts = [strategy: :one_for_one, name: MyStore.Supervisor]
        Supervisor.start_link(children, opts)
      end

  """

  require OpenTelemetry.Tracer
  alias OpenTelemetry.Span
  alias OpentelemetryPhoenix.Reason

  @tracer_id :opentelemetry_phoenix

  @typedoc "Setup options"
  @type opts :: [endpoint_prefix()]

  @typedoc "The endpoint prefix in your endpoint. Defaults to `[:phoenix, :endpoint]`"
  @type endpoint_prefix :: {:endpoint_prefix, [atom()]}

  @typedoc """
  Config for span setup.
  To not trace certain paths, pass a list of paths in ignore_paths e.g. ["/metrics", "/healthz"]
  Note: match on path is exact i.e. ["/user/:id"] would not match path /user/123, but ["/user/123"] would match
  """
  @type config :: %{ignore_paths: list(String.t())}

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts(), config()) :: :ok
  def setup(opts \\ [], config \\ %{ignore_paths: []}) do
    opts = ensure_opts(opts)

    attach_endpoint_start_handler(opts, config)
    attach_endpoint_stop_handler(opts, config)
    attach_router_start_handler()
    attach_router_dispatch_exception_handler()

    :ok
  end

  defp ensure_opts(opts), do: Keyword.merge(default_opts(), opts)

  defp default_opts do
    [endpoint_prefix: [:phoenix, :endpoint]]
  end

  @doc false
  def attach_endpoint_start_handler(opts, config) do
    :telemetry.attach(
      {__MODULE__, :endpoint_start},
      opts[:endpoint_prefix] ++ [:start],
      fn event, measurements, meta, _config ->
        __MODULE__.handle_endpoint_start(event, measurements, meta, config)
      end,
      %{}
    )
  end

  @doc false
  def attach_endpoint_stop_handler(opts, config) do
    :telemetry.attach(
      {__MODULE__, :endpoint_stop},
      opts[:endpoint_prefix] ++ [:stop],
      fn event, measurements, meta, _config ->
        __MODULE__.handle_endpoint_stop(event, measurements, meta, config)
      end,
      %{}
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

  @doc false
  def handle_endpoint_start(_event, _measurements, %{conn: %{adapter: adapter} = conn} = meta, config) do
    :otel_propagator_text_map.extract(conn.req_headers)

    peer_data = Plug.Conn.get_peer_data(conn)

    user_agent = header_value(conn, "user-agent")
    peer_ip = Map.get(peer_data, :address)

    # Turn of all tracing for any routes passed in the config 'ignore_paths' property
    ignore_path? = maybe_ignore_path(conn, config)

    attributes = [
      "http.client_ip": client_ip(conn),
      "http.flavor": http_flavor(adapter),
      "http.host": conn.host,
      "http.method": conn.method,
      "http.scheme": "#{conn.scheme}",
      "http.target": conn.request_path,
      "http.user_agent": user_agent,
      "net.host.ip": to_string(:inet_parse.ntoa(conn.remote_ip)),
      "net.host.port": conn.port,
      "net.peer.ip": to_string(:inet_parse.ntoa(peer_ip)),
      "net.peer.port": peer_data.port,
      "net.transport": :"IP.TCP"
    ]

    # if the request_path is not in the ignore_paths option, then start the span, otherwise don't
    case ignore_path? do
      false ->
        # start the span with a default name. Route name isn't known until router dispatch
        OpentelemetryTelemetry.start_telemetry_span(@tracer_id, "HTTP #{conn.method}", meta, %{
          kind: :server
        })
        |> Span.set_attributes(attributes)

      true ->
        :noop
    end
  end

  @doc false
  def handle_endpoint_stop(_event, _measurements, %{conn: conn} = meta, config) do
    ignore_path? = maybe_ignore_path(conn, config)

    # if the request_path is not in the ignore_paths option, then its parent span was created
    # can be stopped. Otherwise, no-op
    case ignore_path? do
      false ->
        # ensure the correct span is current and update the status
        ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

        Span.set_attribute(ctx, :"http.status_code", conn.status)

        if conn.status >= 400 do
          Span.set_status(ctx, OpenTelemetry.status(:error, ""))
        end

        # end the Phoenix span
        OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)

      true ->
        :noop
    end
  end

  defp maybe_ignore_path(%Plug.Conn{request_path: request_path} = _conn, %{ignore_paths: ignore_paths})
       when is_list(ignore_paths) and is_binary(request_path) do
    Enum.any?(ignore_paths, fn path -> path == request_path end)
  end

  @doc false
  def handle_router_dispatch_start(_event, _measurements, meta, _config) do
    attributes = [
      "phoenix.plug": meta.plug,
      "phoenix.action": meta.plug_opts,
      "http.route": meta.route
    ]

    # Add more info that we now know about but don't close the span
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)
    Span.update_name(ctx, "#{meta.route}")
    Span.set_attributes(ctx, attributes)
  end

  @doc false
  def handle_router_dispatch_exception(
        _event,
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = meta,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    {[reason: reason], attrs} =
      Reason.normalize(reason)
      |> Keyword.split([:reason])

    # try to normalize all errors to Elixir exceptions
    exception = Exception.normalize(kind, reason, stacktrace)

    # record exception and mark the span as errored
    Span.record_exception(ctx, exception, stacktrace, attrs)
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))

    # do not close the span as endpoint stop will still be called with
    # more info, including the status code, which is nil at this stage
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
end
