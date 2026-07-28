defmodule OpentelemetryReq do
  alias OpenTelemetry.Ctx

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes

  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SemanticConventions.Trace
  require Trace
  require Tracer
  require Logger

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
                      Opt-in and experimental attributes. Use semantic conventions library to ensure compatibility, e.g. `[{HTTPAttributes.http_request_body_size(), true}]`

                      #{Enum.map_join(opt_ins, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    propagate_trace_headers: [
                      type: :boolean,
                      default: false,
                      doc: "Trace headers will be propagated"
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
                      type: {:or, [:atom, nil, :string]},
                      default: nil,
                      doc: "User defined span name override"
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
  Wraps a Req request in an opentelemetry span.

  Spans are not created until the request is completed or errored.

  ## Req Path Params

  It is strongly encouraged to use the [`put_path_params` step](https://hexdocs.pm/req/Req.Steps.html#put_path_params/1) option.
  This allows the span name to include the `{target}` portion of the span name described in the
  [HTTP Span Name guidelines](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#name).

  > #### Requirements {: .info}
  >
  > * `path_params` option should be set along with a templated path. Only `:colon` style is supported
  > * `URLAttributes.url_template()` opt-in attribute must be set to `true`

  ## Semantic Conventions

  All available required and recommended [Client HTTP Span](https://opentelemetry.io/docs/specs/semconv/http/http-spans/#http-client) semantic conventions are implemented.
  Supported opt-in and experimental attributes can be configured using the `opt_in_attrs` option.

  ## Options

  ### Opt-in Semantic Convention Attributes

  Otel SemConv requires users to explicitly opt in for any attribute with a
  requirement level of `opt-in` or `experimental`. To ensure compatibility, always use the
  SemConv attribute.

  Example:
  ```
  client =
    Req.new()
    |> OpentelemetryReq.attach(
      opt_in_attrs: [SemConv.URLAttributes.url_template()]
    )

  client
  |> Req.get(
    url: "/api/users/:user_id",
    path_params: [user_id: user_id]
  )
  ```

  Request and response header attributes are opt-in and can be set with the
  `request_header_attrs` and `response_header_attrs` options. Values should be lower-case.

  ### Trace Header Propagation

  By default, trace propagation headers are not injected to requests. There are
  two options available to propagate trace headers:

    * set `propagate_trace_headers` option to `true` when attaching or in the call
    * manually use `:otel_propagator_text_map.inject/1`

  Example:
  ```
  client =
    Req.new()
    |> OpentelemetryReq.attach(propagate_trace_headers: true)

  # or

  client =
    Req.new()
    |> OpentelemetryReq.attach()

  Req.get(client, "/", propagate_trace_headers: true)
  ```

  ### Span Name Override

  The span name can be overridden by setting the `span_name` option in the call.

  Example:
  ```
  client =
    Req.new()
    |> OpentelemetryReq.attach()

  Req.get(client, "/", span_name: "custom")
  ```

  > #### Option Precedence {: .info}
  >
  > Options passed in a request take precedence over those passed in `attach/1`.
  >
  """

  @spec attach(Req.Request.t(), options()) :: Req.Request.t()
  def attach(%Req.Request{} = request, options \\ []) do
    config =
      options
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})
      |> then(fn config ->
        if Enum.member?(config.opt_in_attrs, URLAttributes.url_template()) do
          Map.put(config, :url_template_enabled, true)
        else
          Map.put(config, :url_template_enabled, false)
        end
      end)

    request
    |> Req.Request.put_private(:otel, config)
    |> Req.Request.register_options([
      :propagate_trace_headers,
      :request_header_attrs,
      :response_header_attrs,
      :span_name
    ])
    |> Req.Request.append_request_steps(
      start_span: &start_span/1,
      put_trace_headers: &propagate_trace_headers/1
    )
    |> Req.Request.prepend_response_steps(otel_end_span: &end_span/1)
    |> Req.Request.prepend_error_steps(otel_end_span: &end_errored_span/1)
  end

  defp start_span(request) do
    span_name = span_name(request)

    attrs = build_req_attrs(request)

    parent_ctx = OpenTelemetry.Ctx.get_current()
    Process.put(:otel_parent_ctx, parent_ctx)

    Tracer.start_span(span_name, %{
      attributes: attrs,
      kind: :client
    })
    |> Tracer.set_current_span()

    request
  end

  defp end_span({request, %Req.Response{} = response}) do
    config = Req.Request.get_private(request, :otel)

    opt_in =
      %{
        HTTPAttributes.http_response_body_size() => extract_response_body_size(response)
      }
      |> Map.take(config.opt_in_attrs)

    if response.status >= 400 do
      Tracer.set_status(OpenTelemetry.status(:error, ""))

      %{
        HTTPAttributes.http_response_status_code() => response.status,
        ErrorAttributes.error_type() => to_string(response.status)
      }
      |> set_resp_header_attrs(response, request.options)
      |> Map.merge(opt_in)
      |> Tracer.set_attributes()
    else
      %{
        HTTPAttributes.http_response_status_code() => response.status
      }
      |> set_resp_header_attrs(response, request.options)
      |> Map.merge(opt_in)
      |> Tracer.set_attributes()
    end

    OpenTelemetry.Tracer.end_span()

    Process.delete(:otel_parent_ctx)
    |> OpenTelemetry.Ctx.attach()

    {request, response}
  end

  defp end_errored_span({request, exception}) do
    Tracer.set_status(OpenTelemetry.status(:error, format_exception(exception)))

    Tracer.set_attributes(%{
      ErrorAttributes.error_type() => exception.__struct__
    })

    Tracer.end_span()

    Process.delete(:otel_parent_ctx)
    |> Ctx.attach()

    {request, exception}
  end

  defp format_exception(%{__exception__: true} = exception) do
    Exception.message(exception)
  end

  defp format_exception(_), do: ""

  defp span_name(request) do
    case request.options[:span_name] do
      nil ->
        config = Req.Request.get_private(request, :otel)

        if config.span_name do
          config.span_name
        else
          method = parse_method(request.method)

          if config.url_template_enabled do
            case Req.Request.get_private(request, :path_params_template) do
              nil -> method
              params_template -> "#{method} #{params_template}"
            end
          else
            method
          end
        end

      span_name ->
        span_name
    end
  end

  defp build_req_attrs(request) do
    uri = request.url
    url = sanitize_url(uri)
    config = Req.Request.get_private(request, :otel)

    opt_in = %{
      HTTPAttributes.http_request_body_size() => extract_request_body_size(request),
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => extract_scheme(uri),
      URLAttributes.url_template() => extract_url_template(request),
      UserAgentAttributes.user_agent_original() => extract_user_agent(request)
    }

    %{
      HTTPAttributes.http_request_method() => parse_method(request.method),
      ServerAttributes.server_address() => uri.host,
      ServerAttributes.server_port() => extract_port(uri),
      URLAttributes.url_full() => url
    }
    |> set_retry_count(request)
    |> set_req_header_attrs(request)
    |> then(fn attrs ->
      opt_in
      |> Map.take(config.opt_in_attrs)
      |> Map.merge(attrs)
    end)
  end

  defp sanitize_url(uri) do
    %{uri | userinfo: nil}
    |> URI.to_string()
  end

  defp extract_port(%{port: port}) when is_integer(port), do: port

  defp extract_port(%{scheme: scheme}) do
    case scheme do
      nil -> 80
      "http" -> 80
      "https" -> 443
      _ -> 80
    end
  end

  defp extract_scheme(%{scheme: scheme}) do
    case scheme do
      nil -> :http
      "http" -> :http
      "https" -> :https
      _ -> :http
    end
  end

  defp extract_url_template(request) do
    Req.Request.get_private(request, :path_params_template, "")
  end

  defp extract_user_agent(request) do
    case Req.Request.get_header(request, "user-agent") do
      [] ->
        ""

      [user_agent | _] ->
        user_agent
    end
  end

  defp extract_response_body_size(req) do
    case Req.Response.get_header(req, "content-length") do
      [] ->
        0

      [length_str | _] when is_binary(length_str) ->
        # Req sets this as a string but should be an integer
        # https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length
        # https://opentelemetry.io/docs/specs/semconv/attributes-registry/http
        String.to_integer(length_str)
    end
  end

  defp extract_request_body_size(req) do
    case Req.Request.get_header(req, "content-length") do
      [] ->
        0

      [length_str | _] when is_binary(length_str) ->
        # Req sets this as a string but should be an integer
        # https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length
        # https://opentelemetry.io/docs/specs/semconv/attributes-registry/http
        String.to_integer(length_str)
    end
  end

  defp set_retry_count(attrs, req) do
    retry_count = Req.Request.get_private(req, :req_retry_count, 0)

    if retry_count > 0 do
      Map.put(attrs, HTTPAttributes.http_request_resend_count(), retry_count)
    else
      attrs
    end
  end

  defp parse_method(method) do
    case method do
      :connect -> HTTPAttributes.http_request_method_values().connect
      :delete -> HTTPAttributes.http_request_method_values().delete
      :get -> HTTPAttributes.http_request_method_values().get
      :head -> HTTPAttributes.http_request_method_values().head
      :options -> HTTPAttributes.http_request_method_values().options
      :patch -> HTTPAttributes.http_request_method_values().patch
      :post -> HTTPAttributes.http_request_method_values().post
      :put -> HTTPAttributes.http_request_method_values().put
      :trace -> HTTPAttributes.http_request_method_values().trace
    end
  end

  defp propagate_trace_headers(request) do
    should_inject =
      Req.Request.get_option(
        request,
        :propagate_trace_headers,
        Req.Request.get_private(request, :otel)[:propagate_trace_headers]
      )

    if should_inject do
      Req.Request.put_headers(request, :otel_propagator_text_map.inject([]))
    else
      request
    end
  end

  defp set_req_header_attrs(attrs, req) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :request,
        req.headers,
        Map.get(req.options, :request_header_attrs, [])
      )
    )
  end

  defp set_resp_header_attrs(attrs, resp, options) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :response,
        resp.headers,
        Map.get(options, :response_header_attrs, [])
      )
    )
  end
end
