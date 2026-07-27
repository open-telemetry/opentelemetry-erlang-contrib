defmodule OpentelemetryProcessPropagator.Task.Supervisor do
  @moduledoc """
  `OpentelemetryProcessPropagator.Task.Supervisor` provides a set of extensions
  to the `Task.Supervisor` module to reduce some boilerplate in propagating OpenTelemetry
  contexts across process boundaries. Since these are extensions rather
  than a replacement of Elixir's module, this library can be aliased
  into a file without concern for creating spans where you do not want them.

  Each `Task.Supervisor` function is replicated with two variants: `*_with_span`
  and `*_with_linked_span`. Each of these variations has a specific use case.
  The original implementation for each function automatically propagates the
  current context.

  * `*` - propagates the current context
  * `*_with_span` - propagates the current context and starts a new child span.
  * `*_with_linked_span` - propagates the current context and starts a new linked span.

  > #### Module Redefinement {: .info}
  >
  > This module does not redefine the `Task.Supervisor` module, instead providing a wrapper of the module,
  > so this functionality will not globally modify the default behavior of the `Task` module.
  """

  alias OpentelemetryProcessPropagator.Task.Wrapper
  require OpenTelemetry.Tracer

  @doc false
  defdelegate child_spec(opts), to: Task.Supervisor

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.Supervisor.async/3` for more information.
  """
  @spec async(Supervisor.supervisor(), (-> any())) :: Task.t()
  def async(supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.()
      end,
      options
    )
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.Supervisor.async/5` for more information.
  """
  @spec async(Supervisor.supervisor(), module(), atom(), [term()]) :: Task.t()
  def async(supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async(supervisor, Wrapper, :with_ctx, [ctx, {module, function_name, args}], options)
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.Supervisor.async/3` for more information.
  """
  @spec async_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any())
        ) :: Task.t()
  def async_with_span(name, start_opts, supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.Supervisor.async/5` for more information.
  """
  @spec async_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_with_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async(
      supervisor,
      Wrapper,
      :with_span,
      [name, start_opts, ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.Supervisor.async/3` for more information.
  """
  @spec async_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any())
        ) :: Task.t()
  def async_with_linked_span(name, start_opts, supervisor, fun, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async(
      supervisor,
      fn ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.Supervisor.async/5` for more information.
  """
  @spec async_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_with_linked_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async(
      supervisor,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      options
    )
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.Supervisor.async_nolink/3` for more information.
  """
  @spec async_nolink(Supervisor.supervisor(), (-> any())) :: Task.t()
  def async_nolink(supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_nolink(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.()
      end,
      options
    )
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.Supervisor.async_nolink/5` for more information.
  """
  @spec async_nolink(Supervisor.supervisor(), module(), atom(), [term()]) :: Task.t()
  def async_nolink(supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_nolink(supervisor, Wrapper, :with_ctx, [ctx, {module, function_name, args}], options)
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.Supervisor.async_nolink/3` for more information.
  """
  @spec async_nolink_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any())
        ) :: Task.t()
  def async_nolink_with_span(name, start_opts, supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_nolink(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.Supervisor.async_nolink/5` for more information.
  """
  @spec async_nolink_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_nolink_with_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_nolink(
      supervisor,
      Wrapper,
      :with_span,
      [name, start_opts, ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.Supervisor.async_nolink/3` for more information.
  """
  @spec async_nolink_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any())
        ) :: Task.t()
  def async_nolink_with_linked_span(name, start_opts, supervisor, fun, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_nolink(
      supervisor,
      fn ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.Supervisor.async_nolink/5` for more information.
  """
  @spec async_nolink_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_nolink_with_linked_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_nolink(
      supervisor,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with the current `t:OpenTelemetry.Ctx.t/0`
  attached.

  See `Task.Supervisor.async_stream/4` for more information.
  """
  @spec async_stream(
          Supervisor.supervisor(),
          Enumerable.t(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream(supervisor, enumerable, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.(arg)
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with the
  current `t:OpenTelemetry.Ctx.t/0` attached.

  See `Task.Supervisor.async_stream/6` for more information.
  """
  @spec async_stream(
          Supervisor.supervisor(),
          Enumerable.t(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream(supervisor, enumerable, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      Wrapper,
      :with_ctx,
      [ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new child span.

  See `Task.Supervisor.async_stream/4` for more information.
  """
  @spec async_stream_with_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_span(supervisor, enumerable, name, start_opts, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.(arg)
        end
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new child span.

  See `Task.Supervisor.async_stream/6` for more information.
  """
  @spec async_stream_with_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_span(supervisor, enumerable, name, start_opts, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      Wrapper,
      :with_span,
      [name, start_opts, ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new linked span.

  See `Task.Supervisor.async_stream/4` for more information.
  """
  @spec async_stream_with_linked_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_linked_span(supervisor, enumerable, name, start_opts, fun, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      fn arg ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.(arg)
        end
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new linked span.

  See `Task.Supervisor.async_stream/6` for more information.
  """
  @spec async_stream_with_linked_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_linked_span(
        supervisor,
        enumerable,
        name,
        start_opts,
        module,
        function_name,
        args,
        options \\ []
      ) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_stream(
      supervisor,
      enumerable,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with the current `t:OpenTelemetry.Ctx.t/0`
  attached.

  See `Task.Supervisor.async_stream_nolink/4` for more information.
  """
  @spec async_stream_nolink(
          Supervisor.supervisor(),
          Enumerable.t(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink(supervisor, enumerable, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.(arg)
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with the
  current `t:OpenTelemetry.Ctx.t/0` attached.

  See `Task.Supervisor.async_stream_nolink/6` for more information.
  """
  @spec async_stream_nolink(
          Supervisor.supervisor(),
          Enumerable.t(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink(supervisor, enumerable, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      Wrapper,
      :with_ctx,
      [ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new child span.

  See `Task.Supervisor.async_stream_nolink/4` for more information.
  """
  @spec async_stream_nolink_with_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink_with_span(supervisor, enumerable, name, start_opts, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.(arg)
        end
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new child span.

  See `Task.Supervisor.async_stream_nolink/6` for more information.
  """
  @spec async_stream_nolink_with_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink_with_span(
        supervisor,
        enumerable,
        name,
        start_opts,
        module,
        function_name,
        args,
        options \\ []
      ) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      Wrapper,
      :with_span,
      [name, start_opts, ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new linked span.

  See `Task.Supervisor.async_stream_nolink/4` for more information.
  """
  @spec async_stream_nolink_with_linked_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink_with_linked_span(supervisor, enumerable, name, start_opts, fun, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      fn arg ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.(arg)
        end
      end,
      options
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new linked span.

  See `Task.Supervisor.async_stream_nolink/6` for more information.
  """
  @spec async_stream_nolink_with_linked_span(
          Supervisor.supervisor(),
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_nolink_with_linked_span(
        supervisor,
        enumerable,
        name,
        start_opts,
        module,
        function_name,
        args,
        options \\ []
      ) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.async_stream_nolink(
      supervisor,
      enumerable,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      options
    )
  end

  @doc """
  Starts a task as a child of the given `supervisor` with the
  current `t:OpenTelemetry.Ctx.t/0`.

  See `Task.Supervisor.start_child/3` for more information.
  """
  @spec start_child(
          Supervisor.supervisor(),
          (-> any()),
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child(supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.start_child(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.()
      end,
      options
    )
  end

  @doc """
  Starts a task as a child of the given `supervisor` with the
  current `t:OpenTelemetry.Ctx.t/0`.

  See `Task.Supervisor.start_child/5` for more information.
  """
  @spec start_child(
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child(supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.start_child(supervisor, Wrapper, :with_ctx, [ctx, {module, function_name, args}], options)
  end

  @doc """
  Starts a task as a child of the given `supervisor` in a new child span.

  See `Task.Supervisor.start_child/3` for more information.
  """
  @spec start_child_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any()),
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child_with_span(name, start_opts, supervisor, fun, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.start_child(
      supervisor,
      fn ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task as a child of the given `supervisor` in a new child span.

  See `Task.Supervisor.start_child/5` for more information.
  """
  @spec start_child_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child_with_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.Supervisor.start_child(
      supervisor,
      Wrapper,
      :with_span,
      [name, start_opts, ctx, {module, function_name, args}],
      options
    )
  end

  @doc """
  Starts a task as a child of the given `supervisor` in a new linked span.

  See `Task.Supervisor.start_child/3` for more information.
  """
  @spec start_child_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          (-> any()),
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child_with_linked_span(name, start_opts, supervisor, fun, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.start_child(
      supervisor,
      fn ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.()
        end
      end,
      options
    )
  end

  @doc """
  Starts a task as a child of the given `supervisor` in a new linked span.

  See `Task.Supervisor.start_child/5` for more information.
  """
  @spec start_child_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          Supervisor.supervisor(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: DynamicSupervisor.on_start_child()
  def start_child_with_linked_span(name, start_opts, supervisor, module, function_name, args, options \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.Supervisor.start_child(
      supervisor,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      options
    )
  end

  defdelegate children(supervisor), to: Task.Supervisor
  defdelegate start_link(options \\ []), to: Task.Supervisor
  defdelegate terminate_child(supervisor, pid), to: Task.Supervisor
end
