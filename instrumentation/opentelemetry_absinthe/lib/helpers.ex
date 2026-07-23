defmodule OpentelemetryAbsinthe.Helpers do
  @moduledoc """
  OpenTelemetry-friendly alternatives of Absinthe.Resolution.Helpers functions

  Some of the standard absinthe resolution helpers, like `batch` or `async`,
  are not "opentelemetry-friendly": the resolvers, when invoked, lose the active span
  and break the trace propagation.

  This module defines compatible alternatives that can be used in the same way,
  but don't lose the trace information.
  """

  alias Absinthe.Middleware.Batch
  require OpenTelemetry.Tracer

  @doc """
  Works like Absinthe.Resolution.Helpers.batch, but preserves the active span.

  The function supplied to the `batch` helper is executed in a Task by Absinthe,
  which means that the erlang opentelemetry SDK would lose track of the currently
  active span, because they are kept in a pdict.

  To work around this, you can just replace `batch` with `batch_keep_span`,
  and the active span will be automatically passed and reset as the active one
  inside the batch function.
  """
  @spec batch_keep_span(Batch.batch_fun(), any(), Batch.post_batch_fun()) ::
          {:middleware, Batch, term}
  @spec batch_keep_span(
          Batch.batch_fun(),
          any(),
          Batch.post_batch_fun(),
          opts :: [{:timeout, pos_integer}]
        ) :: {:middleware, Batch, term}

  def batch_keep_span(batch_key, batch_data, post_batch_fn) do
    batch_keep_span(batch_key, batch_data, post_batch_fn, [])
  end

  def batch_keep_span({module, func}, batch_data, post_batch_fn, opts) do
    batch_keep_span({module, func, []}, batch_data, post_batch_fn, opts)
  end

  def batch_keep_span({module, func, param}, batch_data, post_batch_fn, opts) do
    span_ctx = OpenTelemetry.Tracer.current_span_ctx()
    batch_key = {__MODULE__, :batch_fun_wrapper, {{module, func, param}, span_ctx}}
    batch_config = {batch_key, batch_data, post_batch_fn, opts}
    {:middleware, Absinthe.Middleware.Batch, batch_config}
  end

  @doc """
  Wrapper around the "real" batch function used by `batch_keep_span`

  Takes the passed span and sets it as the active one, then calls the original
  batch function with the original parameter.
  """
  def batch_fun_wrapper({{module, func, param}, span}, aggregate) do
    OpenTelemetry.Tracer.set_current_span(span)
    apply(module, func, [param, aggregate])
  end
end
