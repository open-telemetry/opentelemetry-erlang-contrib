defmodule OpentelemetryProcessPropagator.Task do
  require OpenTelemetry.Tracer

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

  def async_with_ctx(fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      fun.()
    end)
  end

  def async_with_span(name, start_opts, fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.async(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      OpenTelemetry.Tracer.with_span name, start_opts do
        fun.()
      end
    end)
  end

  def async_with_linked_span(name, start_opts, fun) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.async(fn ->
      link = OpenTelemetry.link(parent)

      OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
        fun.()
      end
    end)
  end

  def start_with_span(name, start_opts, fun) do
    ctx = OpenTelemetry.Ctx.get_current()

    Task.start(fn ->
      OpenTelemetry.Ctx.attach(ctx)

      OpenTelemetry.Tracer.with_span name, start_opts do
        fun.()
      end
    end)
  end

  def start_with_linked_span(name, start_opts, fun) do
    parent = OpenTelemetry.Tracer.current_span_ctx()

    Task.start(fn ->
      link = OpenTelemetry.link(parent)

      OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, [link]) do
        fun.()
      end
    end)
  end

  defdelegate async_stream(enumerable, fun), to: Task
  defdelegate async_stream(enumerable, fun, options), to: Task

  defdelegate await(task), to: Task
  defdelegate await(task, timeout), to: Task

  defdelegate await_many(tasks), to: Task
  defdelegate await_many(tasks, timeout), to: Task

  defdelegate child_spec(arg), to: Task

  defdelegate completed(result), to: Task

  defdelegate ignore(task), to: Task

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
