defmodule Tesla.Middleware.OpenTelemetry do
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes

  require OpenTelemetry.Tracer, as: Tracer

  @behaviour Tesla.Middleware

  opt_ins = [
    HTTPAttributes.http_request_body_size(),
    HTTPAttributes.http_response_body_size(),
    NetworkAttributes.network_transport(),
    URLAttributes.url_scheme(),
    URLAttributes.url_template(),
    UserAgentAttributes.user_agent_original()
  ]

  @options_schema NimbleOptions.new!(
                    opt_in_attrs: [
                      type: {:list, {:in, opt_ins}},
                      default: [],
                      type_spec: quote(do: opt_in_attrs()),
                      doc: """
                      Opt-in and experimental attributes. Use semantic conventions library to ensure compatibility, e.g. `[HTTPAttributes.http_request_body_size()]`

                      #{Enum.map_join(opt_ins, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    extra_attrs: [
                      type: :map,
                      default: %{},
                      doc: "User-defined attributes included on all spans"
                    ],
                    propagate_trace_headers: [
                      type: :boolean,
                      default: true,
                      doc: "Inject distributed tracing headers into outgoing requests"
                    ],
                    request_header_attrs: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of request headers to add as attributes. (lowercase)"
                    ],
                    response_header_attrs: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of response headers to add as attributes. (lowercase)"
                    ],
                    span_name: [
                      type: {:or, [nil, :string, {:fun, 1}]},
                      default: nil,
                      doc:
                        "User defined span name override. Can be a string or function that takes Tesla.Env"
                    ],
                    mark_status_ok: [
                      type: {:list, :integer},
                      default: [],
                      doc: "List of HTTP error status codes to mark as :ok instead of :error"
                    ]
                  )

  @typedoc "Use semantic conventions library to ensure compatibility, e.g. `HTTPAttributes.http_request_body_size()`"
  @type opt_in_attr() ::
          unquote(HTTPAttributes.http_request_body_size())
          | unquote(HTTPAttributes.http_response_body_size())
          | unquote(NetworkAttributes.network_transport())
          | unquote(URLAttributes.url_scheme())
          | unquote(URLAttributes.url_template())
          | unquote(UserAgentAttributes.user_agent_original())

  @type opt_in_attrs() :: [opt_in_attr()]

  @type options() :: [unquote(NimbleOptions.option_typespec(@options_schema))]

  @moduledoc """
  Creates OpenTelemetry spans and injects tracing headers into HTTP requests

  When used with `Tesla.Middleware.PathParams`, the span name will be created
  based on the provided path. Without it, the span name follow OpenTelemetry
  standards and use just the method name, if not being overridden by opts.

  NOTE: This middleware needs to come before `Tesla.Middleware.PathParams`

  ## Semantic Conventions

  All available required and recommended [Client HTTP Span](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client) semantic conventions are implemented.
  Supported opt-in and experimental attributes can be configured using the `opt_in_attrs` option.

  ## Options

  #{NimbleOptions.docs(@options_schema)}

  ## Tesla Path Params

  It is strongly encouraged to use the [`Tesla.Middleware.PathParams`](https://hexdocs.pm/tesla/Tesla.Middleware.PathParams.html) middleware.
  This allows the span name to include the `{target}` portion of the span name described in the
  [HTTP Span Name guidelines](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#name).

  > #### Requirements {: .info}
  >
  > * `path_params` option should be set along with a templated path
  > * `URLAttributes.url_template()` opt-in attribute must be included in `opt_in_attrs`
  > * OpenTelemetry middleware must come **before** PathParams middleware

  Example:
  ```elixir
  client =
    Tesla.client([
      {Tesla.Middleware.OpenTelemetry,
        opt_in_attrs: [URLAttributes.url_template()]},
      Tesla.Middleware.PathParams
    ])

  Tesla.get(client, "/users/:id", opts: [path_params: [id: "123"]])
  ```

  ## Trace Header Propagation

  By default, trace propagation headers are injected into requests. You can disable this
  by setting `propagate_trace_headers: false`.

  Example:
  ```elixir
  # Disable propagation
  client =
    Tesla.client([
      {Tesla.Middleware.OpenTelemetry, propagate_trace_headers: false}
    ])
  ```

  ## Span Name Override

  The span name can be overridden by setting the `span_name` option. It can be a static
  string or a function that takes the `Tesla.Env` and returns a string.

  Example:
  ```elixir
  client =
    Tesla.client([
      {Tesla.Middleware.OpenTelemetry, span_name: "custom-operation"}
    ])

  # or with a function
  client =
    Tesla.client([
      {Tesla.Middleware.OpenTelemetry, span_name: fn env -> "op-\#{env.method}" end}
    ])
  ```
  """

  def call(env, next, opts) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})

    span_name = get_span_name(env, config.span_name)

    Tracer.with_span span_name, %{kind: :client} do
      env
      |> maybe_put_additional_ok_statuses(config.mark_status_ok)
      |> maybe_propagate(config.propagate_trace_headers)
      |> set_req_span_attributes(config)
      |> Tesla.run(next)
      |> set_resp_span_attributes(config)
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
      nil -> http_method(env.method)
      _ -> "#{http_method(env.method)} #{URI.parse(env.url).path}"
    end
  end

  defp maybe_propagate(env, false), do: env

  defp maybe_propagate(env, true) do
    :otel_propagator_text_map.inject(
      :opentelemetry.get_text_map_injector(),
      env,
      &Tesla.put_header(&3, &1, &2)
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
        Tracer.set_attribute(ErrorAttributes.error_type(), "#{status}")
        :error
      end

    span_status
    |> OpenTelemetry.status("")
    |> Tracer.set_status()

    {:ok, env}
  end

  defp handle_result({:error, {Tesla.Middleware.FollowRedirects, :too_many_redirects}} = result) do
    Tracer.set_attribute(
      ErrorAttributes.error_type(),
      Tesla.Middleware.FollowRedirects
    )

    Tracer.set_status(OpenTelemetry.status(:error, "Too many redirects"))

    result
  end

  defp handle_result({:ok, env}) do
    {:ok, env}
  end

  defp handle_result({:error, reason}) do
    Tracer.set_attribute(ErrorAttributes.error_type(), get_error_struct(reason))
    Tracer.set_status(OpenTelemetry.status(:error, format_error(reason)))

    {:error, reason}
  end

  defp set_req_span_attributes(
         %Tesla.Env{method: method, url: url, headers: headers} = env,
         config
       ) do
    uri = URI.parse(url)

    config.extra_attrs
    |> Map.merge(%{
      HTTPAttributes.http_request_method() => http_method(method),
      ServerAttributes.server_address() => uri.host,
      ServerAttributes.server_port() => extract_port(uri)
    })
    |> add_opt_in_req_attrs(env, config)
    |> add_req_headers(headers, config)
    |> Tracer.set_attributes()

    env
  end

  defp set_resp_span_attributes(
         {:ok, %Tesla.Env{url: url, query: query, status: status_code, headers: headers} = env},
         config
       ) do
    full_url =
      url
      |> Tesla.build_url(query)
      |> sanitize_url()

    %{
      URLAttributes.url_full() => full_url,
      HTTPAttributes.http_response_status_code() => status_code
    }
    |> add_opt_in_resp_attrs(env, config)
    |> add_resp_headers(headers, config)
    |> Tracer.set_attributes()

    {:ok, env}
  end

  defp set_resp_span_attributes({:error, _} = result, _config) do
    result
  end

  defp add_opt_in_req_attrs(attrs, env, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    uri = URI.parse(env.url)

    %{
      HTTPAttributes.http_request_body_size() =>
        parse_integer_header(env.headers, "content-length", 0),
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => parse_scheme(uri.scheme),
      URLAttributes.url_template() => get_url_template(env),
      UserAgentAttributes.user_agent_original() => get_header(env.headers, "user-agent", "")
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_req_attrs(attrs, _env, _config), do: attrs

  defp add_opt_in_resp_attrs(attrs, env, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    %{
      HTTPAttributes.http_response_body_size() =>
        parse_integer_header(env.headers, "content-length", 0)
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_resp_attrs(attrs, _env, _config), do: attrs

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

  defp add_req_headers(attrs, _req_headers, _config), do: attrs

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

  defp add_resp_headers(attrs, _resp_headers, _config), do: attrs

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

  defp parse_integer_header(headers, header_name, default) do
    case get_header(headers, header_name, nil) do
      nil -> default
      value when is_binary(value) -> String.to_integer(value)
      value when is_integer(value) -> value
    end
  end

  defp extract_port(%{port: port}) when is_integer(port), do: port

  defp extract_port(%{scheme: scheme}) do
    case scheme do
      "https" -> 443
      _ -> 80
    end
  end

  defp sanitize_url(url) do
    uri = URI.parse(url)
    %{uri | userinfo: nil} |> URI.to_string()
  end

  defp parse_scheme(nil), do: :http
  defp parse_scheme("http"), do: :http
  defp parse_scheme("https"), do: :https
  defp parse_scheme(_), do: :http

  defp http_method(:connect), do: HTTPAttributes.http_request_method_values().connect
  defp http_method(:delete), do: HTTPAttributes.http_request_method_values().delete
  defp http_method(:get), do: HTTPAttributes.http_request_method_values().get
  defp http_method(:head), do: HTTPAttributes.http_request_method_values().head
  defp http_method(:options), do: HTTPAttributes.http_request_method_values().options
  defp http_method(:patch), do: HTTPAttributes.http_request_method_values().patch
  defp http_method(:post), do: HTTPAttributes.http_request_method_values().post
  defp http_method(:put), do: HTTPAttributes.http_request_method_values().put
  defp http_method(:trace), do: HTTPAttributes.http_request_method_values().trace

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)

  defp get_error_struct(%{__struct__: struct}), do: struct
  defp get_error_struct(_), do: ""
end
