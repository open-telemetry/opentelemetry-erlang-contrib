defmodule Tesla.Middleware.OpenTelemetry do
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes

  require OpenTelemetry.Tracer, as: Tracer

  @behaviour Tesla.Middleware

  @method_other HTTPAttributes.http_request_method_values().other

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
                      type: {:or, [nil, :string]},
                      default: nil,
                      doc:
                        "Static low-cardinality span name target. The span name becomes `{method} {span_name}`."
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
  Creates OpenTelemetry spans and injects tracing headers into HTTP requests for
  the [Tesla](https://hexdocs.pm/tesla) HTTP client.

  ## Semantic Conventions

  All available required and recommended [Client HTTP Span](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client)
  semantic conventions are implemented. Supported opt-in and experimental
  attributes can be configured using the `opt_in_attrs` option.

  ## Middleware Ordering

  Middleware ordering affects which data is available to this middleware.
  The recommended stack:

  ```elixir
  Tesla.client([
    {Tesla.Middleware.Retry, max_retries: 3},
    {Tesla.Middleware.FollowRedirects, max_redirects: 5},
    {Tesla.Middleware.BaseUrl, "https://api.example.com"},
    {Tesla.Middleware.Headers, [{"authorization", "Bearer ..."}]},
    Tesla.Middleware.KeepRequest,
    Tesla.Middleware.PathParams,
    {Tesla.Middleware.OpenTelemetry, opt_in_attrs: [URLAttributes.url_template()]},
  ])
  ```

  Why this order matters:

  * **Retry/FollowRedirects before OTel** — each retry or redirect re-enters OTel, creating
    a separate span per wire attempt with `http.request.resend_count`. This matches the
    SemConv recommendation and ensures `url.full` reflects the originally requested URL.
  * **BaseUrl/Headers before OTel** — ensures `url.full` has the complete URL and headers
    are available for capture.
  * **KeepRequest before PathParams** — preserves the template URL (e.g. `/users/:id`) in
    `opts[:req_url]` before PathParams resolves it. OTel uses this for the low-cardinality
    span name and `url.template` attribute.
  * **PathParams before OTel** — `env.url` is fully resolved when OTel reads it, so `url.full`
    is correct on all code paths including errors.

  > #### Legacy ordering {: .info}
  >
  > Placing OTel **before** PathParams is also supported. In that case, OTel detects
  > `opts[:path_params]` and resolves the URL itself. However, the recommended ordering
  > above is preferred as it produces the most accurate attributes.

  ## Options

  #{NimbleOptions.docs(@options_schema)}

  ## Tesla Path Params

  It is strongly encouraged to use [`Tesla.Middleware.PathParams`](https://hexdocs.pm/tesla/Tesla.Middleware.PathParams.html)
  with [`Tesla.Middleware.KeepRequest`](https://hexdocs.pm/tesla/Tesla.Middleware.KeepRequest.html).
  This allows the span name to include the `{target}` portion described in the
  [HTTP Span Name guidelines](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#name).

  ```elixir
  client =
    Tesla.client([
      Tesla.Middleware.KeepRequest,
      Tesla.Middleware.PathParams,
      {Tesla.Middleware.OpenTelemetry,
        opt_in_attrs: [URLAttributes.url_template()]},
    ])

  Tesla.get(client, "/users/:id", opts: [path_params: [id: "123"]])
  # span name: "GET /users/:id"
  # url.full:  "http://example.com/users/123"
  ```

  ## Trace Header Propagation

  By default, trace propagation headers are injected into requests. Disable with:

  ```elixir
  {Tesla.Middleware.OpenTelemetry, propagate_trace_headers: false}
  ```

  ## Span Name Override

  The span name target can be overridden with a static low-cardinality string.
  The span name becomes `{method} {span_name}`:

  ```elixir
  {Tesla.Middleware.OpenTelemetry, span_name: "my-service"}
  # produces span name: "GET my-service"
  ```
  """

  def call(env, next, opts) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})

    span_name = get_span_name(env, config.span_name)
    start_attrs = build_start_attrs(env, config)

    Tracer.with_span span_name, %{kind: :client, attributes: start_attrs} do
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
    "#{span_method(env.method)} #{span_name}"
  end

  defp get_span_name(env, _) do
    case get_template_path(env) do
      nil -> span_method(env.method)
      path -> "#{span_method(env.method)} #{path}"
    end
  end

  defp get_template_path(%{opts: opts} = env) do
    cond do
      opts[:req_url] != nil -> URI.parse(opts[:req_url]).path
      opts[:path_params] != nil -> URI.parse(env.url).path
      true -> nil
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

  defp build_start_attrs(%Tesla.Env{method: method, url: url, query: query} = env, config) do
    uri = URI.parse(url)

    req_url =
      url
      |> Tesla.build_url(query)
      |> sanitize_url()

    mapped_method = http_method(method)

    attrs =
      Map.merge(config.extra_attrs, %{
        HTTPAttributes.http_request_method() => mapped_method,
        ServerAttributes.server_address() => uri.host,
        ServerAttributes.server_port() => extract_port(uri),
        URLAttributes.url_full() => req_url
      })

    attrs =
      if mapped_method == @method_other do
        Map.put(attrs, HTTPAttributes.http_request_method_original(), method_to_string(method))
      else
        attrs
      end

    maybe_add_resend_count(attrs, env)
  end

  defp maybe_add_resend_count(attrs, %Tesla.Env{opts: opts}) do
    case Keyword.get(opts, :retry_count) do
      count when is_integer(count) and count > 0 ->
        Map.put(attrs, HTTPAttributes.http_request_resend_count(), count)

      _ ->
        attrs
    end
  end

  defp set_req_span_attributes(%Tesla.Env{headers: headers} = env, config) do
    %{}
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
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => parse_scheme(uri.scheme)
    }
    |> maybe_put(HTTPAttributes.http_request_body_size(), parse_content_length(env))
    |> maybe_put(URLAttributes.url_template(), get_url_template(env))
    |> maybe_put(UserAgentAttributes.user_agent_original(), Tesla.get_header(env, "user-agent"))
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_req_attrs(attrs, _env, _config), do: attrs

  defp add_opt_in_resp_attrs(attrs, env, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    %{}
    |> maybe_put(HTTPAttributes.http_response_body_size(), parse_content_length(env))
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
    get_template_path(env)
  end

  defp parse_content_length(env) do
    case Tesla.get_header(env, "content-length") do
      nil -> nil
      value when is_binary(value) -> String.to_integer(value)
      value when is_integer(value) -> value
    end
  end

  defp extract_port(%{port: port}) when is_integer(port), do: port
  defp extract_port(%{scheme: "https"}), do: 443
  defp extract_port(_), do: 80

  defp sanitize_url(url) do
    uri = URI.parse(url)

    case uri.userinfo do
      nil -> URI.to_string(uri)
      _ -> %{uri | userinfo: "REDACTED:REDACTED"} |> URI.to_string()
    end
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

  # TODO: replace with HTTPAttributes.http_request_method_values().query when opentelemetry_semantic_conventions adds it
  defp http_method(:query), do: :QUERY
  defp http_method(:trace), do: HTTPAttributes.http_request_method_values().trace
  defp http_method(_), do: @method_other

  defp span_method(method) do
    case http_method(method) do
      @method_other -> "HTTP"
      known -> known
    end
  end

  defp method_to_string(method) when is_atom(method),
    do: method |> Atom.to_string() |> String.upcase()

  defp method_to_string(method) when is_binary(method), do: String.upcase(method)

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)

  defp get_error_struct(%{__struct__: struct}), do: struct
  defp get_error_struct(reason) when is_atom(reason), do: reason
  defp get_error_struct(_), do: ErrorAttributes.error_type_values().other

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, _key, ""), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
