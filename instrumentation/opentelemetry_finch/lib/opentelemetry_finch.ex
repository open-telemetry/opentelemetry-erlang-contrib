defmodule OpentelemetryFinch do
  @moduledoc """
  OpentelemetryFinch uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
  create `OpenTelemetry` spans.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryFinch.setup()

        # ...
      end

  ## Semantic Conventions

  Module follows [Client HTTP Span v1.27](https://github.com/open-telemetry/semantic-conventions/blob/v1.27.0/docs/http/http-spans.md) semantic conventions.
  Otel configuration should be provided as a map in the `:otel` key of the `Finch.Request.private` map.
  See the headings below for examples.

  ### Span name

  The span name can be as follows in order of precedence:

  - `{method}` - default name if no other options are specified
  - `{span_name}` - custom span name defined with the `span_name` option
  - `{method} {url_template}` - custom span name defined with the `url_template` options

  Example:
  ```elixir
  Finch.build(:get, url)
  |> Finch.Request.put_private(:otel, %{span_name: "custom_span_name"})
  |> Finch.request(HttpFinch)
  ```

  ### Opt-in Semantic Convention Attributes

  Otel SemConv requires users to explicitly opt in for any attribute with a
  requirement level of `opt-in` or `experimental`. To ensure compatibility, always use the
  SemConv attribute.

  Example:
  ```elixir
  Finch.build(:get, url)
  |> Finch.Request.put_private(
    :otel,
    %{opt_in_attrs: [SemConv.URLAttributes.url_template()]}
  )
  |> Finch.request(HttpFinch)
  ```

  Request and response header attributes are opt-in and can be set with the
  `request_header_attrs` and `response_header_attrs` options. Values should be lower-case.

  Example:
  ```elixir
  Finch.build(:get, url)
  |> Finch.Request.put_private(:otel, %{response_header_attrs: ["content-length"]})
  |> Finch.request(HttpFinch)
  ```
  """

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes

  require OpenTelemetry.Tracer

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
                      Opt-in and experimental attributes.
                      Use semantic conventions library to ensure compatibility, e.g. `[HTTPAttributes.http_request_body_size()]`

                      #{Enum.map_join(opt_ins, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    request_header_attrs: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of request headers to add as attributes (lowercase)"
                    ],
                    response_header_attrs: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of response headers to add as attributes (lowercase)"
                    ],
                    span_name: [
                      type: {:or, [:atom, nil, :string]},
                      default: nil,
                      doc: "User defined span name override"
                    ],
                    url_template: [
                      type: {:or, [nil, :string]},
                      default: nil,
                      doc: "URL template to use for span name and opt-in attribute"
                    ]
                  )

  @type opt_in_attr() ::
          unquote(HTTPAttributes.http_request_body_size())
          | unquote(HTTPAttributes.http_response_body_size())
          | unquote(NetworkAttributes.network_transport())
          | unquote(URLAttributes.url_scheme())
          | unquote(URLAttributes.url_template())
          | unquote(UserAgentAttributes.user_agent_original())

  @type opt_in_attrs() :: [opt_in_attr()]

  @typedoc "Setup options"
  @type opts :: []

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts()) :: :ok
  def setup(_opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :request_stop},
      [:finch, :request, :stop],
      &__MODULE__.handle_request_stop/4,
      %{}
    )
  end

  @doc false
  def handle_request_stop(_event, measurements, meta, _config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration
    otel_config = get_otel_config(meta.request.private)
    span_name = span_name(meta.request, otel_config)

    s =
      OpenTelemetry.Tracer.start_span(span_name, %{
        start_time: start_time,
        attributes: build_attrs(meta, otel_config),
        kind: :client
      })

    set_span_status(s, meta.result)

    OpenTelemetry.Span.end_span(s)
  end

  defp get_otel_config(request_private) do
    request_private
    |> Map.get(:otel, %{})
    |> NimbleOptions.validate!(@options_schema)
  end

  defp span_name(request, %{span_name: span_name}) when is_binary(span_name),
    do: "#{request.method} #{span_name}"

  defp span_name(request, %{url_template: url_template}) when is_binary(url_template),
    do: "#{request.method} #{url_template}"

  defp span_name(request, _), do: "#{request.method}"

  defp build_attrs(meta, otel_config) do
    Map.merge(
      build_req_attrs(meta.request, otel_config),
      build_resp_attrs(meta.result, otel_config)
    )
  end

  defp build_req_attrs(request, otel_config) do
    url =
      build_url(
        request.scheme,
        request.host,
        request.port,
        request.path,
        request.query
      )

    %{
      HTTPAttributes.http_request_method() => request.method,
      ServerAttributes.server_address() => request.host,
      ServerAttributes.server_port() => request.port,
      URLAttributes.url_full() => url
    }
    |> add_req_header_attrs(request, otel_config)
    |> add_opt_in_req_attrs(request, otel_config)
  end

  defp add_opt_in_req_attrs(attrs, request, %{opt_in_attrs: [_ | _] = opt_in_attrs} = otel_config) do
    %{
      HTTPAttributes.http_request_body_size() => extract_request_body_size(request),
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => request.scheme,
      URLAttributes.url_template() => extract_url_template(otel_config),
      UserAgentAttributes.user_agent_original() => extract_user_agent(request)
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_req_attrs(attrs, _request, _otel_config), do: attrs

  defp build_resp_attrs({:ok, response} = result, otel_config) do
    %{
      HTTPAttributes.http_response_status_code() => response.status
    }
    |> maybe_add_error_type(result)
    |> add_resp_header_attrs(response, otel_config)
    |> add_opt_in_resp_attrs(response, otel_config)
  end

  defp build_resp_attrs({:error, _} = result, _otel_config) do
    maybe_add_error_type(%{}, result)
  end

  defp add_opt_in_resp_attrs(attrs, response, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    %{
      HTTPAttributes.http_response_body_size() => extract_response_body_size(response)
    }
    |> Map.take(opt_in_attrs)
    |> then(&Map.merge(attrs, &1))
  end

  defp add_opt_in_resp_attrs(attrs, _response, _otel_config), do: attrs

  defp add_req_header_attrs(attrs, req, otel_config) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :request,
        req.headers,
        Map.get(otel_config, :request_header_attrs, [])
      )
    )
  end

  defp add_resp_header_attrs(attrs, resp, otel_config) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :response,
        resp.headers,
        Map.get(otel_config, :response_header_attrs, [])
      )
    )
  end

  defp extract_request_body_size(request) do
    case get_header(request.headers, "content-length") do
      [] ->
        0

      [length_str | _] when is_binary(length_str) ->
        String.to_integer(length_str)
    end
  end

  defp extract_url_template(otel_config) do
    Map.get(otel_config, :url_template, "")
  end

  defp extract_user_agent(request) do
    case get_header(request.headers, "user-agent") do
      [] ->
        ""

      [user_agent | _] ->
        user_agent
    end
  end

  defp extract_response_body_size(response) do
    case get_header(response.headers, "content-length") do
      [] ->
        0

      [length_str | _] when is_binary(length_str) ->
        String.to_integer(length_str)
    end
  end

  defp maybe_add_error_type(attrs, {:ok, %{status: status}}) when status >= 400 do
    Map.put(attrs, ErrorAttributes.error_type(), to_string(status))
  end

  defp maybe_add_error_type(attrs, {:error, reason}) do
    Map.put(attrs, ErrorAttributes.error_type(), format_error(reason))
  end

  defp maybe_add_error_type(attrs, _), do: attrs

  defp set_span_status(span, {:ok, %{status: status}}) when status >= 400 do
    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:error, to_string(status)))
  end

  defp set_span_status(span, {:error, reason}) do
    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:error, format_error(reason)))
  end

  defp set_span_status(span, _result) do
    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:ok, ""))
  end

  defp get_header(headers, header_name) do
    headers
    |> Enum.filter(fn {k, _} -> k == header_name end)
    |> Enum.map(fn {_, v} -> v end)
  end

  defp build_url(scheme, host, port, path, query) do
    "#{scheme}://#{host}:#{port}#{path}" |> append_query(query)
  end

  defp append_query(url, nil), do: url
  defp append_query(url, ""), do: url
  defp append_query(url, query), do: "#{url}?#{query}"

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)
end
