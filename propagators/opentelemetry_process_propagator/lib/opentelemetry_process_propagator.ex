defmodule OpentelemetryProcessPropagator do
  @moduledoc """
  `OpentelemetryProcessPropagator` provides helpers for dealing
  with context propagation across process boundaries.

  ## Context Propagation

  Erlang and Elixir do not have a mechanism for transparently passing
  context between processes. This requires the user to explicitly
  pass data between processes. In order to continue a trace across
  processes, the user must start a new span and pass it to the
  spawned process.

  ```
  span_ctx = OpenTelemetry.Tracer.start_span("child")
  ctx = OpenTelemetry.Ctx.get_current()

  task =
    Task.async(fn ->
      OpenTelemetry.Ctx.attach(ctx)
      OpenTelemetry.Tracer.set_current_span(span_ctx)
      # do work here

      OpenTelemetry.Tracer.end_span(span_ctx)
    end)

  _result = Task.await(task)
  ```

  ### Reverse Propagation

  It's not always possible to have full control over traces, such as
  when using `telemetry` events emitted from a library you don't control
  to create a span. In such cases, a mechanism to fetch a context from a
  calling process is necessary. This is effectively context propagation
  in reverse.

  As an example, Ecto uses the `Task` module to execute preloads which are
  each a separate query. Since a task is a spawned process, creating an otel
  span results in orphan spans. To correctly connect these spans we must
  find the otel context which spawned the process.

  > #### Spawning Processes {: .warning}
  >
  > Processes spawned using `:erlang.spawn/1` and related functions _do not_
  > include ancestor information in the spawned process.
  >
  > Processes spawned by `:proc_lib.spawn/1` and related functions _do_ propagate
  > the pid of the spawning process to the spawned process. These pids are stored
  > under the `:"$ancestors` key.
  >
  > If there is a possibility you may need to reverse propagate from a spawned process,
  > prefer using `:proc_lib` over `:erlang`.

  > #### Elixir Task {: .info}
  >
  > The Elixir `Task` module uses the `:"$callers` key.


  ## Usage

  Example of using `fetch_parent_ctx/1` to find a parent context.

  ```elixir
  OpenTelemetry.with_span :span_started_in_your_app do
    # some span being created in a process spawned by a library
    # you don't control, e.g. Ecto preloads

    Task.async(fn ->
      parent_ctx = OpentelemetryProcessPropagator.fetch_parent_ctx(:"$callers")

      OpenTelemetry.Ctx.attach(parent_ctx)

      attrs = %{some_attr: :from_telemetry_event}

      span =
        OpenTelemetry.Tracer.start_span(:span_created_in_lib, %{attributes: attrs})

      OpenTelemetry.Span.end_span(span)
    end)
  end
  ```

  ```erlang
  ?with_span(span_started_in_your_app, #{}, fun() ->
    %% some span being created in a process spawned by a library
    %% you don't control

    proc_lib:spawn_link(fun() ->
      Tracer = opentelemetry:get_tracer(test_tracer),
      ParentCtx = opentelemetry_process_propagator:fetch_parent_ctx(),
      otel_ctx:attach(ParentCtx),
      Span = otel_tracer:start_span(Tracer, span_created_in_lib, #{}),
      otel_tracer:end_span(Span).
    ).
  end
  ```

  """

  @doc """
  Attempt to fetch an otel context from a given pid.
  """
  @spec fetch_ctx(pid) :: OpenTelemetry.span_ctx() | %{}
  defdelegate fetch_ctx(pid), to: :opentelemetry_process_propagator

  @doc """
  Attempt to find an otel context in the spawning process.

  This is equivalent to calling `fetch_parent_ctx(1, :"$ancestors")`
  """
  @spec fetch_parent_ctx() :: OpenTelemetry.span_ctx() | %{}
  defdelegate fetch_parent_ctx(), to: :opentelemetry_process_propagator

  @doc """
  Attempt to find an otel context in a spawning process within `n` number of parent
  processes
  """
  @spec fetch_parent_ctx(non_neg_integer()) :: OpenTelemetry.span_ctx() | %{}
  defdelegate fetch_parent_ctx(depth), to: :opentelemetry_process_propagator

  @doc """
  Attempt to find an otel context under a given process dictionary key
  within `n` number of parent processes. The first context found will be
  returned.
  """
  @spec fetch_parent_ctx(non_neg_integer(), atom()) :: OpenTelemetry.span_ctx() | %{}
  defdelegate fetch_parent_ctx(max_depth, key), to: :opentelemetry_process_propagator

  @doc """
  Attempt to get an otel context starting with the current process before attempting to
  check parent processes.

  See `fetch_parent_ctx/0` for more information.
  """
  @spec get_ctx() :: OpenTelemetry.span_ctx() | %{}
  defdelegate get_ctx(), to: :opentelemetry_process_propagator

  @doc """
  Attempt to get an otel context starting with the current process before attempting to
  check in a spawning process within `n` number of parent processes. The first context found
  will be returned.

  See `fetch_parent_ctx/1` for more information.
  """
  @spec get_ctx(non_neg_integer()) :: OpenTelemetry.span_ctx() | %{}
  defdelegate get_ctx(depth), to: :opentelemetry_process_propagator

  @doc """
  Attempt to get an otel context starting with the current process before attempting to
  check in a spawning process within `n` number of parent processes. The first context found
  will be returned.

  See `fetch_parent_ctx/2` for more information.
  """
  @spec get_ctx(non_neg_integer(), atom()) :: OpenTelemetry.span_ctx() | %{}
  defdelegate get_ctx(max_depth, key), to: :opentelemetry_process_propagator
end
