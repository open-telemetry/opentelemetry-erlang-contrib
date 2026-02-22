defmodule OpentelemetryOban do
  @moduledoc """
  OpentelemetryOban uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
  create `OpenTelemetry` spans from Oban events.

  Supported events include job start/stop and also when an exception is raised.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryOban.setup()

        # ...
      end
  """

  alias Ecto.Changeset
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemConv.Incubating.MessagingAttributes

  require Logger
  require OpenTelemetry.Tracer

  @type job_options() ::
          [unquote(NimbleOptions.option_typespec(OpentelemetryOban.JobHandler.options_schema()))]
          | :disabled

  @type plugin_options() :: [] | :disabled

  @options_schema [
    job: [
      type:
        {:or,
         [
           {:keyword_list, OpentelemetryOban.JobHandler.options_schema()},
           {:in, [:disabled]}
         ]},
      type_spec: quote(do: job_options()),
      default: [],
      doc: """
      Job handler configuration. Set to `:disabled` to skip job handler setup. \n\n#{NimbleOptions.docs(OpentelemetryOban.JobHandler.options_schema(), nest_level: 1)}
      """
    ],
    plugin: [
      type: {:in, [[], :disabled]},
      type_spec: quote(do: plugin_options()),
      default: [],
      doc: "Plugin handler configuration. Set to `:disabled` to skip plugin handler setup."
    ]
  ]

  @nimble_options_schema NimbleOptions.new!(@options_schema)

  @doc """
  Initializes and configures telemetry handlers.

  Note that if you don't trace plugins, but inside the plugins, there are spans
  from other instrumentation libraries (e.g. ecto) then these will still be
  traced. This setting controls only the spans that are created by
  opentelemetry_oban.

  ## Options

  #{NimbleOptions.docs(@nimble_options_schema)}
  """
  @spec setup(unquote(NimbleOptions.option_typespec(@options_schema))) :: :ok
  def setup(opts \\ []) do
    opts =
      opts
      |> normalize_legacy_opts()
      |> NimbleOptions.validate!(@nimble_options_schema)
      |> Enum.into(%{})

    if opts.job != :disabled do
      OpentelemetryOban.JobHandler.attach(opts.job)
    end

    if opts.plugin != :disabled do
      OpentelemetryOban.PluginHandler.attach()
    end

    :ok
  end

  def insert(name \\ Oban, changeset, opts \\ [])
  def insert(name, %Changeset{} = changeset, opts) do
    attributes = attributes_before_insert(changeset)
    queue = Changeset.get_field(changeset, :queue)

    OpenTelemetry.Tracer.with_span "send #{queue}", attributes: attributes, kind: :producer do
      changeset = add_tracing_information_to_meta(changeset)

      case Oban.insert(name, changeset, opts) do
        {:ok, job} ->
          OpenTelemetry.Tracer.set_attributes(attributes_after_insert(job))
          {:ok, job}

        other ->
          other
      end
    end
  end

  def insert(name \\ Oban, multi, multi_name, changeset_or_fun, opts \\ []) do
    Oban.insert(name, multi, multi_name, changeset_or_fun, opts)
  end

  def insert!(name \\ Oban, %Changeset{} = changeset, opts \\ []) do
    attributes = attributes_before_insert(changeset)
    queue = Changeset.get_field(changeset, :queue)

    OpenTelemetry.Tracer.with_span "send #{queue}", attributes: attributes, kind: :producer do
      changeset = add_tracing_information_to_meta(changeset)

      try do
        job = Oban.insert!(name, changeset, opts)
        OpenTelemetry.Tracer.set_attributes(attributes_after_insert(job))
        job
      rescue
        exception ->
          ctx = OpenTelemetry.Tracer.current_span_ctx()
          Span.record_exception(ctx, exception, __STACKTRACE__)
          Span.set_status(ctx, OpenTelemetry.status(:error, ""))
          reraise exception, __STACKTRACE__
      end
    end
  end

  def insert_all(name \\ Oban, changesets_or_wrapper, opts \\ [])

  def insert_all(name, %{changesets: changesets}, opts) when is_list(changesets) do
    insert_all(name, changesets, opts)
  end

  def insert_all(name, changesets, opts) when is_list(changesets) do
    # changesets in insert_all can include different workers and different
    # queues. This means we cannot provide much information here, but we can
    # still record the insert and propagate the context information.
    OpenTelemetry.Tracer.with_span "send", kind: :producer do
      changesets = Enum.map(changesets, &add_tracing_information_to_meta/1)
      Oban.insert_all(name, changesets, opts)
    end
  end

  def insert_all(name \\ Oban, multi, multi_name, changesets_or_wrapper, opts \\ []) do
    Oban.insert_all(name, multi, multi_name, changesets_or_wrapper, opts)
  end

  defp normalize_legacy_opts(opts) do
    case Keyword.pop(opts, :trace) do
      {nil, opts} ->
        opts

      {trace, opts} ->
        Logger.warning(
          "OpentelemetryOban.setup/1 :trace option is deprecated, use :job and :plugin options instead"
        )

        opts
        |> disable_unless_traced(trace, :jobs, :job)
        |> disable_unless_traced(trace, :plugins, :plugin)
    end
  end

  defp disable_unless_traced(opts, trace, legacy_key, new_key) do
    if legacy_key in trace do
      opts
    else
      Keyword.put_new(opts, new_key, :disabled)
    end
  end

  defp add_tracing_information_to_meta(changeset) do
    meta = Changeset.get_field(changeset, :meta, %{})

    new_meta =
      []
      |> :otel_propagator_text_map.inject()
      |> Enum.into(meta)

    Changeset.change(changeset, %{meta: new_meta})
  end

  defp attributes_before_insert(changeset) do
    queue = Changeset.get_field(changeset, :queue)
    worker = Changeset.get_field(changeset, :worker)

    %{
      MessagingAttributes.messaging_system() => :oban,
      MessagingAttributes.messaging_operation_name() => "send",
      MessagingAttributes.messaging_client_id() => worker,
      MessagingAttributes.messaging_destination_name() => queue,
      MessagingAttributes.messaging_operation_type() =>
        MessagingAttributes.messaging_operation_type_values().create,
      :"oban.job.worker" => worker
    }
  end

  defp attributes_after_insert(job) do
    %{
      MessagingAttributes.messaging_message_id() => job.id,
      "oban.job.job_id": job.id,
      "oban.job.priority": job.priority,
      "oban.job.max_attempts": job.max_attempts
    }
  end
end
