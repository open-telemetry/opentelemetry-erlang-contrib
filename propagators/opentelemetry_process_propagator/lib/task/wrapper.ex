defmodule OpentelemetryProcessPropagator.Task.Wrapper do
  @moduledoc false
  require OpenTelemetry.Tracer

  @spec with_ctx(OpenTelemetry.Ctx.t(), {module(), atom(), [term()]}) :: any()
  def with_ctx(ctx, {m, f, a}) do
    OpenTelemetry.Ctx.attach(ctx)

    apply(m, f, a)
  end

  # for streams which prepend the value to the given arguments
  @spec with_ctx(term(), OpenTelemetry.Ctx.t(), {module(), atom(), [term()]}) :: any()
  def with_ctx(value, ctx, {m, f, a}) do
    OpenTelemetry.Ctx.attach(ctx)

    apply(m, f, [value | a])
  end

  @spec with_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          OpenTelemetry.Ctx.t(),
          {module(), atom(), [term()]}
        ) :: any()
  def with_span(name, start_opts, ctx, {m, f, a}) do
    OpenTelemetry.Ctx.attach(ctx)

    OpenTelemetry.Tracer.with_span name, start_opts do
      apply(m, f, a)
    end
  end

  # for streams which prepend the value to the given arguments
  @spec with_span(
          term(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          OpenTelemetry.Ctx.t(),
          {module(), atom(), [term()]}
        ) :: any()
  def with_span(value, name, start_opts, ctx, {m, f, a}) do
    OpenTelemetry.Ctx.attach(ctx)

    OpenTelemetry.Tracer.with_span name, start_opts do
      apply(m, f, [value | a])
    end
  end

  @spec with_linked_span(
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          OpenTelemetry.Ctx.t(),
          {module(), atom(), [term()]}
        ) :: any()
  def with_linked_span(name, start_opts, parent, {m, f, a}) do
    links =
      if parent == :undefined do
        []
      else
        [OpenTelemetry.link(parent)]
      end

    OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, links) do
      apply(m, f, a)
    end
  end

  # for streams which prepend the value to the given arguments
  @spec with_linked_span(
          term(),
          OpenTelemetry.span_name(),
          OpenTelemetry.Span.start_opts(),
          OpenTelemetry.Ctx.t(),
          {module(), atom(), [term()]}
        ) :: any()
  def with_linked_span(value, name, start_opts, parent, {m, f, a}) do
    links =
      if parent == :undefined do
        []
      else
        [OpenTelemetry.link(parent)]
      end

    OpenTelemetry.Tracer.with_span name, Map.put(start_opts, :links, links) do
      apply(m, f, [value | a])
    end
  end
end
