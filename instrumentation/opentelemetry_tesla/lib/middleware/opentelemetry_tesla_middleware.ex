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
    - `:opt_in_attrs` - list of opt-in and experimental attributes to be added. Use semantic conventions library to ensure compatibility, e.g. `[HTTPAttributes.http_request_body_size()]`
    - `:request_header_attrs` - list of request headers to be added as attributes, e.g. `["content-type"]`
    - `:response_header_attrs` - list of response headers to be added as attributes, e.g. `["content-length"]`
  """

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes

  require OpenTelemetry.Tracer

  @behaviour Tesla.Middleware

  @otel_opts ~w[span_name propagator mark_status_ok opt_in_attrs request_header_attrs response_header_attrs]a

  def call(env, next, opts) do
    opts = opts |> Keyword.take(@otel_opts) |> Enum.into(%{})
    span_name = get_span_name(env, Map.get(opts, :span_name))

    OpenTelemetry.Tracer.with_span span_name, %{kind: :client} do
      env
      |> maybe_put_additional_ok_statuses(opts[:mark_status_ok])
      |> maybe_propagate(Map.get(opts, :propagator, :opentelemetry.get_text_map_injector()))
      |> set_req_span_attributes(opts)
      |> Tesla.run(next)
      |> set_resp_span_attributes(opts)
      |> handle_result()
    end
  end

  defp get_span_name(env, span_name) when is_binary(span_name) do
    "#{http_method(env.method)} #{span_name}"
  end

  defp get_span_name(env, span_name_fun) when is_function(span_name_fun, 1) do
    "#{http_method(env.method)} #{span_name_fun.(env)}"
  end

  defp get_span_name(env, _) do
    # `path_params` is used by `Tesla.Middleware.PathParams`
    # if `path_params` is not set, the path potentially has high cardinality and we cannot use it in the span name
    # if `path_params` is set, it means that the path is a template and we can use it in the span name
    case env.opts[:path_params] do
      nil -> "#{http_method(env.method)}"
      _ -> "#{http_method(env.method)} #{URI.parse(env.url).path}"
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

  defp handle_result({:ok, %Tesla.Env{status: status, opts: opts} = env}) when status >= 400 do
    span_status =
      if status in Keyword.get(opts, :additional_ok_statuses, []) do
        :ok
      else
        OpenTelemetry.Tracer.set_attribute(ErrorAttributes.error_type(), "#{status}")
        :error
      end

    span_status
    |> OpenTelemetry.status("")
    |> OpenTelemetry.Tracer.set_status()

    {:ok, env}
  end

  defp handle_result({:error, {Tesla.Middleware.FollowRedirects, :too_many_redirects}} = result) do
    OpenTelemetry.Tracer.set_attribute(
      ErrorAttributes.error_type(),
      Tesla.Middleware.FollowRedirects
    )

    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, "Too many redirects"))

    result
  end

  defp handle_result({:ok, env}) do
    {:ok, env}
  end

  defp handle_result({:error, reason}) do
    OpenTelemetry.Tracer.set_attribute(ErrorAttributes.error_type(), get_error_struct(reason))
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, format_error(reason)))

    {:error, reason}
  end

  defp set_req_span_attributes(%Tesla.Env{method: method, url: url, headers: headers} = env, opts) do
    uri = URI.parse(url)

    %{
      HTTPAttributes.http_request_method() => http_method(method),
      ServerAttributes.server_address() => uri.host,
      ServerAttributes.server_port() => uri.port
    }
    |> add_opt_in_req_attrs(env, opts)
    |> add_req_headers(headers, opts)
    |> OpenTelemetry.Tracer.set_attributes()

    env
  end

  defp set_resp_span_attributes(
         {:ok, %Tesla.Env{url: url, query: query, status: status_code, headers: headers} = env},
         opts
       ) do
    url = Tesla.build_url(url, query)

    %{
      # Sets url.full only after the request is completed so that all middleware has been called
      URLAttributes.url_full() => url,
      HTTPAttributes.http_response_status_code() => status_code
    }
    |> add_opt_in_resp_attrs(env, opts)
    |> add_resp_headers(headers, opts)
    |> OpenTelemetry.Tracer.set_attributes()

    {:ok, env}
  end

  defp set_resp_span_attributes({:error, _} = result, _opts) do
    result
  end

  defp add_opt_in_req_attrs(attrs, env, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    uri = URI.parse(env.url)

    %{
      HTTPAttributes.http_request_body_size() => get_header(env.headers, "content-length", "0"),
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => uri.scheme,
      URLAttributes.url_template() => get_url_template(env),
      UserAgentAttributes.user_agent_original() => get_header(env.headers, "user-agent", "")
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_req_attrs(attrs, _uri, _opts), do: attrs

  defp add_opt_in_resp_attrs(attrs, env, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    %{
      HTTPAttributes.http_response_body_size() => get_header(env.headers, "content-length", "0")
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_resp_attrs(attrs, _env, _opts), do: attrs

  defp add_req_headers(
         attrs,
         req_headers,
         %{request_header_attrs: [_ | _] = request_header_attrs}
       ) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :request,
        req_headers,
        request_header_attrs
      )
    )
  end

  defp add_req_headers(attrs, _req_headers, _opts), do: attrs

  defp add_resp_headers(
         attrs,
         resp_headers,
         %{response_header_attrs: [_ | _] = response_header_attrs}
       ) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :response,
        resp_headers,
        response_header_attrs
      )
    )
  end

  defp add_resp_headers(attrs, _resp_headers, _opts), do: attrs

  defp get_url_template(env) do
    case env.opts[:path_params] do
      nil -> ""
      _ -> URI.parse(env.url).path
    end
  end

  defp get_header(headers, header_name, default) do
    headers
    |> Enum.find(fn {k, _v} -> k == header_name end)
    |> case do
      nil -> default
      {_key, value} -> value
    end
  end

  defp http_method(method) do
    method
    |> Atom.to_string()
    |> String.upcase()
  end

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)

  defp get_error_struct(%{__struct__: struct}), do: struct
  defp get_error_struct(_), do: ""
end
