defmodule Tesla.Middleware.OpenTelemetry do
  @moduledoc """
  Creates OpenTelemetry spans and injects tracing headers into HTTP requests

  When used with `Tesla.Middleware.PathParams`, the span name will be created
  based on the provided path. Without it, the span name follow OpenTelemetry
  standards and use just the method name, if not being overriden by opts.

  NOTE: This middleware needs to come before `Tesla.Middleware.PathParams`

  ## Options

    - `:span_name` - override span name. Can be a `String` for a static span name,
    or a function that takes the `Tesla.Env` and returns a `String`

  """
  require OpenTelemetry.Tracer
  @behaviour Tesla.Middleware

  def call(env, next, opts) do
    span_name = get_span_name(env, Keyword.get(opts, :span_name))

    OpenTelemetry.Tracer.with_span span_name, %{kind: :client} do
      env
      |> Tesla.put_headers(:otel_propagator_text_map.inject([]))
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

  defp set_span_attributes({_, %Tesla.Env{} = env} = result) do
    OpenTelemetry.Tracer.set_attributes(build_attrs(env))

    result
  end

  defp set_span_attributes(result) do
    result
  end

  defp handle_result({:ok, %Tesla.Env{status: status} = env}) when status > 400 do
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, ""))

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
      "http.method": http_method(method),
      "http.url": url,
      "http.target": uri.path,
      "http.host": uri.host,
      "http.scheme": uri.scheme,
      "http.status_code": status_code
    }

    maybe_append_content_length(attrs, headers)
  end

  defp maybe_append_content_length(attrs, headers) do
    case Enum.find(headers, fn {k, _v} -> k == "content-length" end) do
      nil ->
        attrs

      {_key, content_length} ->
        Map.put(attrs, :"http.response_content_length", content_length)
    end
  end

  defp http_method(method) do
    method
    |> Atom.to_string()
    |> String.upcase()
  end
end
