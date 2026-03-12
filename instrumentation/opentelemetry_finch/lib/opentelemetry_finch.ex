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

  Module follows [Client HTTP Span v1.39](https://github.com/open-telemetry/semantic-conventions/blob/v1.39.0/docs/http/http-spans.md) semantic conventions.
  Otel configuration should be provided as a map in the `:otel` key of the `Finch.Request.private` map.
  See the headings below for examples.

  ### Span name

  The span name can be as follows in order of precedence:

  - `{method}` - default name if no other options are specified
  - `{method} {span_name}` - when `span_name` option is provided
  - `{method} {url_template}` - when `url_template` option is provided

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
    %{opt_in_attrs: [OpenTelemetry.SemConv.Incubating.URLAttributes.url_template()]}
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

  require Logger
  require OpenTelemetry.Tracer, as: Tracer

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
                      type: {:or, [nil, :string]},
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
    status = extract_status(meta.result)

    s =
      Tracer.start_span(span_name, %{
        start_time: start_time,
        attributes: build_attrs(meta.request, meta.result, status, otel_config),
        kind: :client
      })

    set_span_status(s, meta.result, status)

    OpenTelemetry.Span.end_span(s)
  end

  defp get_otel_config(request_private) do
    raw = Map.get(request_private, :otel, %{})

    case NimbleOptions.validate(raw, @options_schema) do
      {:ok, config} ->
        config

      {:error, %NimbleOptions.ValidationError{} = error} ->
        Logger.warning(
          "OpentelemetryFinch: invalid :otel config ignored, using defaults. " <>
            Exception.message(error)
        )

        NimbleOptions.validate!(%{}, @options_schema)
    end
  end

  defp span_name(request, %{span_name: span_name}) when is_binary(span_name),
    do: "#{request.method} #{span_name}"

  defp span_name(request, %{url_template: url_template}) when is_binary(url_template),
    do: "#{request.method} #{url_template}"

  defp span_name(request, _), do: "#{request.method}"

  defp extract_status({:ok, %{status: status}}) when is_integer(status), do: status
  defp extract_status({:ok, {status, _, _}}) when is_integer(status), do: status
  defp extract_status({:ok, {_, %{status: status}}}) when is_integer(status), do: status
  defp extract_status(_), do: nil

  defp build_attrs(request, result, status, otel_config) do
    Map.merge(
      build_req_attrs(request, otel_config),
      build_resp_attrs(result, status, otel_config)
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
      NetworkAttributes.network_transport() => :tcp,
      URLAttributes.url_scheme() => request.scheme
    }
    |> Map.take(opt_in_attrs)
    |> put_body_size(opt_in_attrs, HTTPAttributes.http_request_body_size(), request.headers)
    |> put_url_template(opt_in_attrs, otel_config)
    |> put_user_agent(opt_in_attrs, request)
    |> Map.merge(attrs)
  end

  defp add_opt_in_req_attrs(attrs, _request, _otel_config), do: attrs

  # Finch.Response — full attribute extraction including headers
  defp build_resp_attrs({:ok, %{status: _, headers: _} = response} = result, status, otel_config) do
    status
    |> resp_status_attrs()
    |> maybe_add_error_type(status, result)
    |> add_resp_header_attrs(response, otel_config)
    |> add_opt_in_resp_attrs(response, otel_config)
  end

  # Streaming accumulator or error — status only
  defp build_resp_attrs(result, status, _otel_config) do
    status
    |> resp_status_attrs()
    |> maybe_add_error_type(status, result)
  end

  defp resp_status_attrs(status) when is_integer(status) do
    %{HTTPAttributes.http_response_status_code() => status}
  end

  defp resp_status_attrs(_status), do: %{}

  defp add_opt_in_resp_attrs(attrs, response, %{opt_in_attrs: [_ | _] = opt_in_attrs}) do
    put_body_size(attrs, opt_in_attrs, HTTPAttributes.http_response_body_size(), response.headers)
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

  defp put_url_template(attrs, opt_in_attrs, %{url_template: url_template})
       when is_binary(url_template) do
    if URLAttributes.url_template() in opt_in_attrs do
      Map.put(attrs, URLAttributes.url_template(), url_template)
    else
      attrs
    end
  end

  defp put_url_template(attrs, _opt_in_attrs, _otel_config), do: attrs

  defp put_user_agent(attrs, opt_in_attrs, request) do
    if UserAgentAttributes.user_agent_original() in opt_in_attrs do
      case get_header(request.headers, "user-agent") do
        [user_agent | _] -> Map.put(attrs, UserAgentAttributes.user_agent_original(), user_agent)
        [] -> attrs
      end
    else
      attrs
    end
  end

  defp put_body_size(attrs, opt_in_attrs, attr_key, headers) do
    if attr_key in opt_in_attrs do
      case parse_content_length(headers) do
        {:ok, size} -> Map.put(attrs, attr_key, size)
        :error -> attrs
      end
    else
      attrs
    end
  end

  defp parse_content_length(headers) do
    case get_header(headers, "content-length") do
      [length_str | _] when is_binary(length_str) ->
        case Integer.parse(length_str) do
          {int, _} -> {:ok, int}
          :error -> :error
        end

      [] ->
        :error
    end
  end

  defp maybe_add_error_type(attrs, status, _result) when is_integer(status) and status >= 400 do
    Map.put(attrs, ErrorAttributes.error_type(), to_string(status))
  end

  defp maybe_add_error_type(attrs, _status, {:error, reason}) do
    Map.put(attrs, ErrorAttributes.error_type(), format_error(reason))
  end

  defp maybe_add_error_type(attrs, _status, _result), do: attrs

  defp set_span_status(span, _result, status) when is_integer(status) and status >= 400 do
    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:error, to_string(status)))
  end

  defp set_span_status(span, {:error, reason}, _status) do
    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:error, format_error(reason)))
  end

  defp set_span_status(_span, _result, _status), do: :ok

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
