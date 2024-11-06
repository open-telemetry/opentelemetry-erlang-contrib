defmodule OpentelemetryBandit do
  # TODOS
  # * add attr stability lookups in semconv with maps
  # * store shadow attrs in conn priv from semconv maps?
  # * perf test:
  #   * map with shadow attrs
  #   * map with shadow attrs with multiple attr ops (memory in particular)

  alias OpenTelemetry.Ctx

  alias OpenTelemetry.SemConv.ClientAttributes
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.URLAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes

  alias OpenTelemetry.Tracer
  require OpenTelemetry.Tracer

  opt_ins = [
    ClientAttributes.client_port(),
    HTTPAttributes.http_request_body_size(),
    HTTPAttributes.http_response_body_size(),
    NetworkAttributes.network_local_address(),
    NetworkAttributes.network_local_port(),
    NetworkAttributes.network_transport()
  ]

  @options_schema NimbleOptions.new!(
                    opt_in_attrs: [
                      type: {:list, {:in, opt_ins}},
                      default: [],
                      type_spec: quote(do: opt_in_attrs()),
                      doc: """
                      Use semantic conventions library to ensure compatability, e.g. `[HTTPAttributes.http_request_body_size()]`

                      #{Enum.map_join(opt_ins, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    handler_id: [
                      type: :atom,
                      default: :otel_bandit,
                      doc: "Only set when running multiple instances on different endpoints"
                    ],
                    client_address_headers: [
                      type: {:list, :string},
                      default: ["forwarded", "x-forwarded-for"],
                      doc: "Headers to use for extracting original client request address info"
                    ],
                    client_headers_sort_fn: [
                      type: {:fun, 2},
                      doc: "Custom client header sort fn. See `otel_http` for more info"
                    ],
                    public_endpoint: [
                      type: :boolean,
                      default: false,
                      doc: "Endpoint is public. Propagated traces will be added as a link."
                    ],
                    public_endpoint_fn: [
                      type: :mfa,
                      default: {__MODULE__, :default_public_endpoint_fn, []},
                      doc: "Default function returns `false`. See docs for more info"
                    ],
                    request_headers: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of request headers to add as attributes. (lowercase)"
                    ],
                    response_headers: [
                      type: {:list, :string},
                      default: [],
                      doc: "List of response headers to add as attributes. (lowercase)"
                    ],
                    scheme_headers: [
                      type: {:list, :string},
                      default: ["forwarded", "x-forwarded-proto"],
                      doc: "Headers to use for extracting original client request scheme"
                    ],
                    scheme_headers_sort_fn: [
                      type: {:fun, 2},
                      doc: "Custom scheme header sort fn. See `otel_http` for more info"
                    ],
                    server_address_headers: [
                      type: {:list, :string},
                      default: ["forwarded", "x-forwarded-host", "host"],
                      doc: "Headers to use for extracting original server address info"
                    ],
                    server_headers_sort_fn: [
                      type: {:fun, 2},
                      doc: "Custom server header sort fn. See `otel_http` for more info"
                    ]
                  )

  @typedoc "Use semantic conventions library to ensure compatability, e.g. `HTTPAttributes.http_request_body_size()`"
  @type opt_in_attr() ::
          unquote(ClientAttributes.client_port())
          | unquote(HTTPAttributes.http_request_body_size())
          | unquote(HTTPAttributes.http_response_body_size())
          | unquote(NetworkAttributes.network_local_address())
          | unquote(NetworkAttributes.network_local_port())
          | unquote(NetworkAttributes.network_transport())

  @type opt_in_attrs() :: [opt_in_attr()]

  @type options() :: [unquote(NimbleOptions.option_typespec(@options_schema))]

  @moduledoc """
  OpenTelemetry instrumentation for Bandit.

  ## Semantic Conventions

  All required and recommended Server HTTP Span semantic conventions are implemented.
  Supported opt-in attributes can be configured using the `opt_in_attrs` option.

  ## Options

  ### Opt-in Semantic Convention Attributes

  Otel SemConv requires users to explicitly opt in for any attribute with a
  requirement level of `opt-in`. To ensure compatability, always use the
  SemConv attribute.

  Example:
  ```
  opt_ins = [SemConv.HTTPAttributes.http_request_body_size()]`
  OpentelemetryBandit.setup(opt_in_attrs: opt_ins)
  ```

  #### Request and Response Headers as Opt-in Attributes

  Request and response header attributes are opt-in and can be set with the
  `request_headers` and `response_headers` options. Values should be lower-case.

  ```
  OpentelemetryBandit.setup(request_headers: ["x-customer-id"])
  ```

  ### Public Endpoint

  Setting an endpoint as public will result in any propagated trace to be added as a link,
  rather than a continuation of an existing trace. The `public_endpoint` option should be set
  to `true` if an endpoint only accepts public traffic to prevent missing root spans. By default,
  the endpoint is handled as non-public, resulting in traces being continued rather than linked.

  In a mixed traffic environment, an MFA can be supplied to determine whether to
  treat a request as public. This function is executed on every request, so refrain
  from expensive operations such as lookups to external systems. The function must
  be a predicate function of arity-2, accepting the `conn` from the request as
  the first argument and user-supplied options as the second. Any dynamic
  information used in comparisons should be supplied at setup time for efficiency.

  Example:
  ```
  defmodule PublicEndpoint do
    def is_public_request?(conn, opts) do
      # return true if request was public
    end
  end

  OpentelemetryBandit.setup(public_endpoint_fn: {PublicEndpoint, :is_public_request?, []})
  ```
  """

  @doc """
  Initializes and configures the telemetry handlers.

  Supported options:\n#{NimbleOptions.docs(@options_schema)}
  """
  @spec setup(any) :: :ok
  def setup(opts \\ []) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})
      |> Map.update!(:scheme_headers, &Enum.reverse/1)
      |> Map.update!(:client_address_headers, &Enum.reverse/1)

    :telemetry.attach_many(
      {__MODULE__, config.handler_id},
      [
        [:bandit, :request, :start],
        [:bandit, :request, :stop],
        [:bandit, :request, :exception]
      ],
      &__MODULE__.handle_request/4,
      config
    )
  end

  @doc false
  def handle_request([:bandit, :request, :start], _measurements, meta, config) do
    handle_request_start(meta, config)
  end

  @doc false
  def handle_request([:bandit, :request, :stop], measurements, meta, config) do
    handle_request_stop(measurements, meta, config)
  end

  @doc false
  def handle_request([:bandit, :request, :exception], _measurements, meta, config) do
    handle_request_exception(meta, config)
  end

  @doc false
  def handle_request_start(%{conn: conn}, config) do
    peer_data = Plug.Conn.get_peer_data(conn)
    request_method = parse_method(conn.method)
    client_address = extract_client_address(conn, config)

    opt_in = %{
      ClientAttributes.client_port() => client_address.port,
      NetworkAttributes.network_local_address() => conn.host,
      NetworkAttributes.network_local_port() => conn.port,
      NetworkAttributes.network_transport() => :tcp
    }

    attrs =
      %{
        ClientAttributes.client_address() => client_address.ip,
        HTTPAttributes.http_request_method() => request_method,
        NetworkAttributes.network_peer_address() => ip_to_string(peer_data.address),
        NetworkAttributes.network_peer_port() => peer_data.port,
        URLAttributes.url_path() => conn.request_path,
        URLAttributes.url_scheme() => extract_scheme(conn, config),
        UserAgentAttributes.user_agent_original() => header_value(conn, "user-agent")
      }
      |> set_network_protocol_attrs(conn)
      |> set_query_string_attr(conn)
      |> set_server_address_attrs(conn, config)
      |> set_req_header_attrs(conn, config)
      |> then(fn attrs ->
        opt_in
        |> Map.take(config.opt_in_attrs)
        |> Map.merge(attrs)
      end)

    name =
      if request_method == HTTPAttributes.http_request_method_values().other,
        do: :HTTP,
        else: request_method

    if public_endpoint?(conn, config) do
      propagated_ctx =
        :otel_propagator_text_map.extract_to(Ctx.new(), conn.req_headers)
        |> Tracer.current_span_ctx()

      # test public endpoint and public endpoint fn
      Tracer.start_span(name, %{
        kind: :server,
        attributes: attrs,
        links: OpenTelemetry.links([propagated_ctx])
      })
      |> Tracer.set_current_span()
    else
      :otel_propagator_text_map.extract(conn.req_headers)

      Tracer.start_span(name, %{kind: :server, attributes: attrs})
      |> Tracer.set_current_span()
    end
  end

  # error with no conn
  def handle_request_start(_meta, _config) do
    Tracer.start_span(:HTTP, %{kind: :server})
    |> Tracer.set_current_span()
  end

  defp public_endpoint?(_conn, %{public_endpoint: true}), do: true

  defp public_endpoint?(conn, %{public_endpoint_fn: {m, f, a}}) do
    apply(m, f, [conn, a])
  end

  @doc false
  def default_public_endpoint_fn(_, _), do: false

  defp set_req_header_attrs(attrs, _conn, %{request_headers: []}), do: attrs

  defp set_req_header_attrs(attrs, conn, %{request_headers: headers}) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :request,
        conn.req_headers,
        headers
      )
    )
  end

  defp set_resp_header_attrs(attrs, _conn, %{response_headers: []}), do: attrs

  defp set_resp_header_attrs(attrs, conn, %{response_headers: headers}) do
    Map.merge(
      attrs,
      :otel_http.extract_headers_attributes(
        :response,
        conn.resp_headers,
        headers
      )
    )
  end

  defp header_value(conn, header) do
    case Plug.Conn.get_req_header(conn, header) do
      [] ->
        ""

      [value | _] ->
        value
    end
  end

  defp set_network_protocol_attrs(attrs, conn) do
    case extract_network_protocol(conn.adapter) do
      {:error, _reason} ->
        attrs

      {:http, version} ->
        Map.put(attrs, NetworkAttributes.network_protocol_version(), version)
    end
  end

  defp set_query_string_attr(attrs, %{query_string: ""}), do: attrs

  defp set_query_string_attr(attrs, %{query_string: query}),
    do: Map.put(attrs, URLAttributes.url_query(), query)

  # server attributes are only set from headers
  # https://opentelemetry.io/docs/specs/semconv/http/http-spans/#setting-serveraddress-and-serverport-attributes
  defp set_server_address_attrs(attrs, conn, config) do
    case extract_server_address(conn, config) do
      %{address: :undefined} ->
        attrs

      %{address: address, port: :undefined} ->
        Map.put(attrs, ServerAttributes.server_address(), address)

      %{address: address, port: port} ->
        Map.merge(
          attrs,
          %{
            ServerAttributes.server_address() => address,
            ServerAttributes.server_port() => port
          }
        )
    end
  end

  defp extract_network_protocol(
         {_adapter_name, %{transport: %{__struct__: Bandit.HTTP1.Socket} = transport}}
       ) do
    case transport.version do
      :"HTTP/1.0" -> {:http, :"1.0"}
      :"HTTP/1.1" -> {:http, :"1.1"}
      nil -> {:error, "Invalid protocol"}
    end
  end

  defp extract_network_protocol({_adapter_name, %{transport: %{__struct__: Bandit.HTTP2.Stream}}}) do
    {:http, :"2"}
  end

  defp extract_network_protocol(_), do: {:error, "Invalid protocol"}

  defp parse_method(method) do
    case method do
      "CONNECT" -> HTTPAttributes.http_request_method_values().connect
      "DELETE" -> HTTPAttributes.http_request_method_values().delete
      "GET" -> HTTPAttributes.http_request_method_values().get
      "HEAD" -> HTTPAttributes.http_request_method_values().head
      "OPTIONS" -> HTTPAttributes.http_request_method_values().options
      "PATCH" -> HTTPAttributes.http_request_method_values().patch
      "POST" -> HTTPAttributes.http_request_method_values().post
      "PUT" -> HTTPAttributes.http_request_method_values().put
      "TRACE" -> HTTPAttributes.http_request_method_values().trace
      _ -> HTTPAttributes.http_request_method_values().other
    end
  end

  defp extract_headers_by_sort(headers, keys) do
    Enum.filter(headers, fn {key, _} -> key in keys end)
  end

  defp extract_scheme(conn, config) do
    scheme_headers =
      conn.req_headers
      |> extract_headers_by_sort(config.scheme_headers)

    case otel_http_extract_scheme(scheme_headers, config[:scheme_headers_sort_fn]) do
      :undefined -> conn.scheme
      scheme -> scheme
    end
  end

  defp otel_http_extract_scheme(headers, nil) do
    :otel_http.extract_scheme(headers)
  end

  defp otel_http_extract_scheme(headers, sort_fn) do
    :otel_http.extract_scheme(headers, sort_fn)
  end

  # client
  defp extract_client_address(conn, config) do
    client_headers =
      conn.req_headers
      |> extract_headers_by_sort(config.client_address_headers)

    case otel_http_extract_client_info(client_headers, config[:client_headers_sort_fn]) do
      %{ip: :undefined} ->
        %{adapter: {Bandit.Adapter, adapter}} = conn

        extract_client_info_from_transport(adapter.transport)

      client_address ->
        client_address
    end
  end

  defp extract_client_info_from_transport(%{__struct__: Bandit.HTTP1.Socket} = transport) do
    start_meta = transport.socket.span.start_metadata

    %{ip: ip_to_string(start_meta.remote_address), port: start_meta.remote_port}
  end

  defp extract_client_info_from_transport(%{__struct__: Bandit.HTTP2.Stream} = transport) do
    {ip, port} = transport.transport_info.peername

    %{ip: ip_to_string(ip), port: port}
  end

  defp otel_http_extract_client_info(headers, nil) do
    :otel_http.extract_client_info(headers)
  end

  defp otel_http_extract_client_info(headers, sort_fn) do
    :otel_http.extract_client_info(headers, sort_fn)
  end

  # Note: bandit parses host/port but not in the required order and isn't
  # limited to only headers per the spec
  # https://github.com/open-telemetry/semantic-conventions/blob/v1.26.0/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes
  # http2/3 pseudo headers unsupported at this time https://datatracker.ietf.org/doc/html/rfc9113#section-8.3.1
  defp extract_server_address(conn, config) do
    conn.req_headers
    |> extract_headers_by_sort(config.server_address_headers)
    |> otel_http_extract_server_info(config[:server_headers_sort_fn])
  end

  defp otel_http_extract_server_info(headers, nil) do
    :otel_http.extract_server_info(headers)
  end

  defp otel_http_extract_server_info(headers, sort_fn) do
    :otel_http.extract_server_info(headers, sort_fn)
  end

  defp ip_to_string(ip) do
    ip |> :inet.ntoa() |> to_string()
  end

  @doc false
  def handle_request_stop(_measurements, %{error: error_message}, _config) do
    Tracer.set_status(OpenTelemetry.status(:error, ""))
    Tracer.set_attribute(ErrorAttributes.error_type(), error_message)
    Tracer.end_span()
    Ctx.clear()
  end

  @doc false
  def handle_request_stop(measurements, %{conn: conn}, config) do
    opt_in =
      %{
        HTTPAttributes.http_request_body_size() => Map.get(measurements, :req_body_bytes, 0),
        HTTPAttributes.http_response_body_size() => Map.get(measurements, :resp_body_bytes, 0)
      }
      |> Map.take(config.opt_in_attrs)

    if conn.status >= 500 do
      Tracer.set_status(OpenTelemetry.status(:error, ""))

      %{
        HTTPAttributes.http_response_status_code() => conn.status,
        ErrorAttributes.error_type() => to_string(conn.status)
      }
      |> set_resp_header_attrs(conn, config)
      |> Map.merge(opt_in)
      |> Tracer.set_attributes()
    else
      %{
        HTTPAttributes.http_response_status_code() => conn.status
      }
      |> set_resp_header_attrs(conn, config)
      |> Map.merge(opt_in)
      |> Tracer.set_attributes()
    end

    Tracer.end_span()
    Ctx.clear()
  end

  @doc false
  def handle_request_exception(meta, config) do
    Tracer.set_status(OpenTelemetry.status(:error, ""))

    Tracer.record_exception(meta.exception, meta.stacktrace)

    # bandit does not set this on the meta but extracts this after the exception
    # telemetry is emitted
    status_code = meta.exception |> Plug.Exception.status() |> Plug.Conn.Status.code()

    %{
      HTTPAttributes.http_response_status_code() => status_code,
      ErrorAttributes.error_type() => meta.exception.__struct__
    }
    |> set_resp_header_attrs(meta.conn, config)
    |> Tracer.set_attributes()

    Tracer.end_span()
    OpenTelemetry.Ctx.clear()
  end
end
