defmodule OpentelemetryAbsinthe.SpanTracker do
  @moduledoc false

  @doc """
  Initialize Span tracking for current process. Must to be called at operation
  start.

  Expects span to be used as "root", presumably the `graphql.execute` span.
  """
  def init do
    root_ctx = OpenTelemetry.Ctx.get_current()
    update_stack(fn _ -> [{[], root_ctx}] end)
  end

  @doc """
  Cleanup Span tracking data from current process dictionary. Must be called
  at operation stop.
  """
  def terminate do
    delete_stack()
  end

  @doc """
  Store context into current process tracker stack, stored inside
  the process dictionary.

  Stores paths in reverse order to ease parent lookup.
  """
  def push_ctx(path) do
    ctx = OpenTelemetry.Ctx.get_current()
    update_stack(&[{:lists.reverse(path), ctx} | &1])
  end

  @doc """
  Find a suitable parent span given path, or `nil` otherwise.
  """
  def find_parent_ctx(path) do
    get_stack() |> find_parent_by_path(:lists.reverse(path))
  end

  @doc """
  Find the "root" span, i.e., `graphql.execute`. Useful to attach middleware
  like batch spans, as they run on separate phases from resolution.
  """
  def root_parent_ctx do
    case List.last(get_parent_stack()) do
      {_path, ctx} -> ctx
      _ -> :undefined
    end
  end

  defp find_parent_by_path(stack, path) do
    stack
    |> Enum.find_value(fn {parent_path, ctx} ->
      if match_parent?(parent_path, path), do: ctx
    end) || :undefined
  end

  # Recursively search if candidate is a subpath of parent. Expects paths to be in
  # reverse order, i.e., [:grandchild, :child, :parent].
  #
  # Short-circuit on paths impossible to match.
  defp match_parent?(parent, candidate)
  defp match_parent?(path, path), do: true
  defp match_parent?(parent, path) when length(parent) >= length(path), do: false
  defp match_parent?(parent, [_ | rest]), do: match_parent?(parent, rest)
  defp match_parent?(_parent, _path), do: false

  @ctx_stack {__MODULE__, :ctx_stack}

  defp get_parent_stack do
    case Process.get(@ctx_stack) do
      nil ->
        pids = Process.get(:"$callers", [])
        Enum.find_value(pids, &fetch_ctx_stack(&1))

      ctx_stack ->
        ctx_stack
    end || []
  end

  defp get_stack do
    Process.get(@ctx_stack) || []
  end

  defp update_stack(fun) do
    Process.put(@ctx_stack, fun.(get_stack()))
  end

  defp delete_stack do
    Process.delete(@ctx_stack)
  end

  defp fetch_ctx_stack(pid) do
    with {_, pdict} <- Process.info(pid, :dictionary),
         {_key, ctx_stack} <- List.keyfind(pdict, @ctx_stack, 0) do
      ctx_stack
    else
      _not_found -> nil
    end
  end
end
