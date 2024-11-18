defmodule OpentelemetryPhoenix do
  @options_schema NimbleOptions.new!(
                    endpoint_prefix: [
                      type: {:list, :atom},
                      default: [:phoenix, :endpoint],
                      doc: "The endpoint prefix in your endpoint."
                    ],
                    adapter: [
                      type: {:in, [:cowboy2, :bandit]},
                      default: :cowboy2,
                      required: true,
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

  #### Adapters

  * `cowboy2` - when using PlugCowboy as your adapter you must add `:opentelemetry_cowboy` to your project
  and pass `adapter: :cowboy2` option when calling setup.
  * `bandit` - when using `Bandit.PhoenixAdapter` as your adapter you must add `:opentelemetry_bandit` to your project
  and pass `adapter: :bandit` option when calling setup

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
  alias OpenTelemetry.SemConv.Incubating.URLAttributes

  alias OpenTelemetry.Tracer

  require OpenTelemetry.Tracer

  @tracer_id __MODULE__

  @typedoc "Setup options"
  @type opts :: [endpoint_prefix() | adapter()]

  @typedoc "The endpoint prefix in your endpoint. Defaults to `[:phoenix, :endpoint]`"
  @type endpoint_prefix :: {:endpoint_prefix, [atom()]}

  @typedoc "The phoenix server adapter being used. Required"
  @type adapter :: {:adapter, :cowboy2 | :bandit}

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts()) :: :ok
  def setup(opts \\ []) do
    opts = NimbleOptions.validate!(opts, @options_schema)

    attach_endpoint_start_handler(opts)
    attach_router_start_handler(opts)

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
  def attach_router_start_handler(_opts) do
    :telemetry.attach(
      {__MODULE__, :router_dispatch_start},
      [:phoenix, :router_dispatch, :start],
      &__MODULE__.handle_router_dispatch_start/4,
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

  # TODO: do we still need exception handling? Only when cowboy?

  @doc false
  def handle_endpoint_start(_event, _measurements, _meta, %{adapter: :bandit}), do: :ok

  def handle_endpoint_start(_event, _measurements, _meta, %{adapter: :cowboy2}) do
    cowboy2_start()
  end

  defp cowboy2_start do
    OpentelemetryProcessPropagator.fetch_parent_ctx()
    |> OpenTelemetry.Ctx.attach()
  end

  @doc false
  def handle_router_dispatch_start(_event, _measurements, meta, _config) do
    attributes = %{
      :"phoenix.plug" => meta.plug,
      :"phoenix.action" => meta.plug_opts,
      URLAttributes.url_template() => meta.route
    }

    Tracer.update_name("#{meta.conn.method} #{meta.route}")
    Tracer.set_attributes(attributes)
  end

  def handle_liveview_event(
        [:phoenix, _live, :mount, :start],
        _measurements,
        %{socket: %{view: live_view}} = meta,
        _handler_configuration
      ) do
    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.mount",
      meta,
      %{kind: :server}
    )
  end

  def handle_liveview_event(
        [:phoenix, _live, :handle_params, :start],
        _measurements,
        %{socket: %{view: live_view}} = meta,
        _handler_configuration
      ) do
    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_params",
      meta,
      %{kind: :server}
    )
  end

  def handle_liveview_event(
        [:phoenix, _live, :handle_event, :start],
        _measurements,
        %{socket: %{view: live_view}, event: event} = meta,
        _handler_configuration
      ) do
    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_event##{event}",
      meta,
      %{kind: :server}
    )
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
end
