defmodule OpentelemetryProcessPropagator.Task do
  @moduledoc """
  `OpentelemetryProcessPropagator.Task` provides a set of extensions
  to the `Task` module to reduce some boilerplate in propagating OpenTelemetry
  contexts across process boundaries. Since these are extensions rather
  than a replacement of Elixir's module, this library can be aliased
  into a file without concern for creating spans where you do not want them.

  Each Task function is replicated with three variants: `*_with_ctx`, `*_with_span`,
  and `*_with_linked_span`. Each of these variations has a specific use case.

  * `*_with_ctx` - propagates the current context without starting a new child span.
  * `*_with_span` - propagates the current context and starts a new child span.
  * `*_with_linked_span` - propagates the current context and starts a new linked span.

  ## Usage

  ```
  defmodule MyApp do
    require OpenTelemetry.Tracer
    alias OpentelemetryProcessPropagator.Task

    def untraced_task do
      Task.async(fn ->
        :ok
      end)
      |> Task.await()
    end

    def traced_task do
      Task.async_with_span(:span_name, %{attributes: %{a: "b"}}, fn ->
        Tracer.set_attribute(:c, "d")
        :ok
      end)
      |> Task.await()
    end
  end
  ```
  """
  alias OpentelemetryProcessPropagator.Task.Wrapper
  require OpenTelemetry.Tracer

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with the current `t:OpenTelemetry.Ctx.t/0`
  attached.

  See `Task.async_stream/3` for more information.
  """
  @spec async_stream_with_ctx(Enumerable.t(), (term() -> term()), keyword()) :: Enumerable.t()
  def async_stream_with_ctx(enumerable, fun, opts \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async_stream(
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        fun.(arg)
      end,
      opts
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with the
  current `t:OpenTelemetry.Ctx.t/0` attached.

  See `Task.async_stream/5` for more information.
  """
  @spec async_stream_with_ctx(
          Enumerable.t(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_ctx(enumerable, module, function_name, args, opts \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async_stream(enumerable, Wrapper, :with_ctx, [ctx, {module, function_name, args}], opts)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new child span.

  See `Task.async_stream/3` for more information.
  """
  @spec async_stream_with_span(
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_span(enumerable, name, start_opts, fun, opts \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async_stream(
      enumerable,
      fn arg ->
        OpenTelemetry.Ctx.attach(ctx)

        OpenTelemetry.Tracer.with_span name, start_opts do
          fun.(arg)
        end
      end,
      opts
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new child span.

  See `Task.async_stream/5` for more information.
  """
  @spec async_stream_with_span(
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_span(enumerable, name, start_opts, module, function_name, args, opts \\ []) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async_stream(enumerable, Wrapper, :with_span, [name, start_opts, ctx, {module, function_name, args}], opts)
  end

  @doc """
  Returns a stream that runs the given function `fun` concurrently
  on each element in `enumerable` with a new linked span.

  See `Task.async_stream/3` for more information.
  """
  @spec async_stream_with_linked_span(
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (term() -> term()),
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_linked_span(enumerable, name, start_opts, fun, opts \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.async_stream(
      enumerable,
      fn arg ->
        link = OpenTelemetry.link(parent)

        OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
          fun.(arg)
        end
      end,
      opts
    )
  end

  @doc """
  Returns a stream where the given function (`module` and `function`)
  is mapped concurrently on each element in `enumerable` with a new linked span.

  See `Task.async_stream/5` for more information.
  """
  @spec async_stream_with_linked_span(
          Enumerable.t(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()],
          keyword()
        ) :: Enumerable.t()
  def async_stream_with_linked_span(enumerable, name, start_opts, module, function_name, args, opts \\ []) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.async_stream(
      enumerable,
      Wrapper,
      :with_linked_span,
      [name, start_opts, parent, {module, function_name, args}],
      opts
    )
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.async/1` for more information.
  """
  @spec async_with_ctx((() -> any())) :: Task.t()
  def async_with_ctx(fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      fun.()
    end)
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0` that can be awaited on.

  See `Task.async/3` for more information.
  """
  @spec async_with_ctx(module(), atom(), [term()]) :: Task.t()
  def async_with_ctx(module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(Wrapper, :with_ctx, [ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.async/1` for more information.
  """
  @spec async_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: Task.t()
  def async_with_span(name, start_opts, fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      OpenTelemetry.Tracer.with_span name, start_opts do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task with a new child span that can be awaited on.

  See `Task.async/3` for more information.
  """
  @spec async_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_with_span(name, start_opts, module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(Wrapper, :with_span, [name, start_opts, ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.async/1` for more information.
  """
  @spec async_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: Task.t()
  def async_with_linked_span(name, start_opts, fun) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.async(fn ->
      link = OpenTelemetry.link(parent)

      OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task with a new linked span that can be awaited on.

  See `Task.async/3` for more information.
  """
  @spec async_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: Task.t()
  def async_with_linked_span(name, start_opts, module, function_name, args) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.async(Wrapper, :with_linked_span, [name, start_opts, parent, {module, function_name, args}])
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0`.

  See `Task.start/1` for more information.
  """
  @spec start_with_ctx((() -> any())) :: {:ok, pid()}
  def start_with_ctx(fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      fun.()
    end)
  end

  @doc """
  Starts a task with the current `t:OpenTelemetry.Ctx.t/0`.

  See `Task.start/3` for more information.
  """
  @spec start_with_ctx(module(), atom(), [term()]) :: {:ok, pid()}
  def start_with_ctx(module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start(Wrapper, :with_ctx, [ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task with a new child span.

  See `Task.start/1` for more information.
  """
  @spec start_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: {:ok, pid()}
  def start_with_span(name, start_opts, fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      OpenTelemetry.Tracer.with_span name, start_opts do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task with a new child span.

  See `Task.start/3` for more information.
  """
  @spec start_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: {:ok, pid()}
  def start_with_span(name, start_opts, module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start(Wrapper, :with_span, [name, start_opts, ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task with a new linked span.

  See `Task.start/1` for more information.
  """
  @spec start_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: {:ok, pid()}
  def start_with_linked_span(name, start_opts, fun) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.start(fn ->
      link = OpenTelemetry.link(parent)

      OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task with a new linked span.

  See `Task.start/3` for more information.
  """
  @spec start_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: {:ok, pid()}
  def start_with_linked_span(name, start_opts, module, function_name, args) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.start(Wrapper, :with_linked_span, [name, start_opts, parent, {module, function_name, args}])
  end

  @doc """
  Starts a task as part of a supervision tree with the given `fun`
  with the current `t:OpenTelemetry.Ctx.t/0` attached.

  See `Task.start_link/1` for more information.
  """
  @spec start_link_with_ctx((() -> any())) :: {:ok, pid()}
  def start_link_with_ctx(fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start_link(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      fun.()
    end)
  end

  @doc """
  Starts a task as part of a supervision tree with the given
  `module`, `function`, and `args` with the current `t:OpenTelemetry.Ctx.t/0`
  attached.

  See `Task.start_link/3` for more information.
  """
  @spec start_link_with_ctx(module(), atom(), [term()]) :: {:ok, pid()}
  def start_link_with_ctx(module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start_link(Wrapper, :with_ctx, [ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task as part of a supervision tree with the given `fun`
  in a new child span.

  See `Task.start_link/1` for more information.
  """
  @spec start_link_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: {:ok, pid()}
  def start_link_with_span(name, start_opts, fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start_link(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      OpenTelemetry.Tracer.with_span name, start_opts do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task as part of a supervision tree with the given
  `module`, `function`, and `args` in a new child span.

  See `Task.start_link/3` for more information.
  """
  @spec start_link_with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: {:ok, pid()}
  def start_link_with_span(name, start_opts, module, function_name, args) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start_link(Wrapper, :with_span, [name, start_opts, ctx, {module, function_name, args}])
  end

  @doc """
  Starts a task as part of a supervision tree with the given `fun`
  in a new linked span.

  See `Task.start_link/1` for more information.
  """
  @spec start_link_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          (() -> any())
        ) :: {:ok, pid()}
  def start_link_with_linked_span(name, start_opts, fun) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.start_link(fn ->
      link = OpenTelemetry.link(parent)

      OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
        fun.()
      end
    end)
  end

  @doc """
  Starts a task as part of a supervision tree with the given
  `module`, `function`, and `args` in a new linked span.

  See `Task.start_link/3` for more information.
  """
  @spec start_link_with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          module(),
          atom(),
          [term()]
        ) :: {:ok, pid()}
  def start_link_with_linked_span(name, start_opts, module, function_name, args) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.start_link(Wrapper, :with_linked_span, [name, start_opts, parent, {module, function_name, args}])
  end

  defdelegate async(fun), to: Task
  defdelegate async(module, fun, args), to: Task

  defdelegate async_stream(enumerable, fun), to: Task
  defdelegate async_stream(enumerable, fun, options), to: Task

  defdelegate async_stream(enumerable, module, function_name, args), to: Task
  defdelegate async_stream(enumerable, module, function_name, args, options), to: Task

  defdelegate await(task), to: Task
  defdelegate await(task, timeout), to: Task

  defdelegate await_many(tasks), to: Task
  defdelegate await_many(tasks, timeout), to: Task

  defdelegate child_spec(arg), to: Task

  if Kernel.function_exported?(Task, :completed, 1) do
    defdelegate completed(result), to: Task
  end

  if Kernel.function_exported?(Task, :ignore, 1) do
    defdelegate ignore(task), to: Task
  end

  defdelegate shutdown(task), to: Task
  defdelegate shutdown(task, timeout), to: Task

  defdelegate start_link(fun), to: Task
  defdelegate start_link(module, function, args), to: Task

  defdelegate start(fun), to: Task
  defdelegate start(module, function, args), to: Task

  defdelegate yield_many(tasks), to: Task
  defdelegate yield_many(tasks, timeout), to: Task

  defdelegate yield(tasks), to: Task
  defdelegate yield(tasks, timeout), to: Task
end
