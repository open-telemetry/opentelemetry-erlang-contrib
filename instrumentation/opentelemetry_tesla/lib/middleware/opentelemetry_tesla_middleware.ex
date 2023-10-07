defmodule Tesla.Middleware.OpenTelemetry do
  @moduledoc """
  Creates OpenTelemetry spans and injects tracing headers into HTTP requests

  When used with `Tesla.Middleware.PathParams`, the span name will be created
  based on the provided path. Without it, the span name follow OpenTelemetry
  standards and use just the method name, if not being overridden by opts.

  NOTE: This middleware needs to come before `Tesla.Middleware.PathParams`

  ## Options

    - `:span_name` - override span name. Can be a `String` for a static span name,
    or a function that takes the `Tesla.Env` and returns a `String`
    - `:propagator` - configures trace headers propagators. Setting it to `:none` disables propagation.
    Any module that implements `:otel_propagator_text_map` can be used.
    Defaults to calling `:otel_propagator_text_map.get_text_map_injector/0`
    - `:mark_status_ok` - configures spans with a list of expected HTTP error codes to be marked as `ok`,
    not as an error-containing spans
  """

  alias OpenTelemetry.SemanticConventions.Trace

  require OpenTelemetry.Tracer
  require Trace

  @behaviour Tesla.Middleware

  def call(env, next, opts) do
    span_name = get_span_name(env, Keyword.get(opts, :span_name))

    OpenTelemetry.Tracer.with_span span_name, %{kind: :client} do
      env
      |> maybe_put_additional_ok_statuses(opts[:mark_status_ok])
      |> maybe_propagate(Keyword.get(opts, :propagator, :opentelemetry.get_text_map_injector()))
      |> Tesla.run(next)
      |> set_span_attributes()
      |> handle_result()
    end
  end

  defp get_span_name(_env, span_name) when is_binary(span_name) do
    span_name
  end

  defp get_span_name(env, span_name_fun) when is_function(span_name_fun, 1) do
    span_name_fun.(env)
  end

  defp get_span_name(env, _) do
    case env.opts[:path_params] do
      nil -> "HTTP #{http_method(env.method)}"
      _ -> URI.parse(env.url).path
    end
  end

  defp maybe_propagate(env, :none), do: env

  defp maybe_propagate(env, propagator) do
    :otel_propagator_text_map.inject(
      propagator,
      env,
      fn key, value, env -> Tesla.put_header(env, key, value) end
    )
  end

  defp maybe_put_additional_ok_statuses(env, [_ | _] = additional_ok_statuses) do
    case env.opts[:additional_ok_statuses] do
      nil -> Tesla.put_opt(env, :additional_ok_statuses, additional_ok_statuses)
      _ -> env
    end
  end

  defp maybe_put_additional_ok_statuses(env, _additional_ok_statuses), do: env

  defp set_span_attributes({_, %Tesla.Env{} = env} = result) do
    OpenTelemetry.Tracer.set_attributes(build_attrs(env))

    result
  end

  defp set_span_attributes(result) do
    result
  end

  defp handle_result({:ok, %Tesla.Env{status: status, opts: opts} = env}) when status >= 400 do
    span_status =
      if status in Keyword.get(opts, :additional_ok_statuses, []), do: :ok, else: :error

    span_status
    |> OpenTelemetry.status("")
    |> OpenTelemetry.Tracer.set_status()

    {:ok, env}
  end

  defp handle_result({:error, {Tesla.Middleware.FollowRedirects, :too_many_redirects}} = result) do
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, ""))

    result
  end

  defp handle_result({:ok, env}) do
    {:ok, env}
  end

  defp handle_result(result) do
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, ""))

    result
  end

  defp build_attrs(%Tesla.Env{
         method: method,
         url: url,
         status: status_code,
         headers: headers,
         query: query
       }) do
    url = Tesla.build_url(url, query)
    uri = URI.parse(url)

    attrs = %{
      Trace.http_method() => http_method(method),
      Trace.http_url() => url,
      Trace.http_target() => uri.path,
      Trace.net_host_name() => uri.host,
      Trace.http_scheme() => uri.scheme,
      Trace.http_status_code() => status_code
    }

    maybe_append_content_length(attrs, headers)
  end

  defp maybe_append_content_length(attrs, headers) do
    case Enum.find(headers, fn {k, _v} -> k == "content-length" end) do
      nil ->
        attrs

      {_key, content_length} ->
        Map.put(attrs, Trace.http_response_content_length(), content_length)
    end
  end

  defp http_method(method) do
    method
    |> Atom.to_string()
    |> String.upcase()
  end
end
