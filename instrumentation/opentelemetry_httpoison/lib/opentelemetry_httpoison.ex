defmodule OpentelemetryHTTPoison do
  @moduledoc """
  OpenTelemetry-instrumented wrapper around HTTPoison.Base

  A client request span is created on request creation, and ended once we get the response.
  http.status and other standard http span attributes are set automatically.
  """

  use HTTPoison.Base

  require OpenTelemetry
  require OpenTelemetry.Span
  require OpenTelemetry.Tracer
  require Record
  require Logger

  alias HTTPoison.Request
  alias OpenTelemetry.Tracer
  alias OpentelemetryHTTPoison.Configuration
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes

  @doc ~S"""
  Configures OpentelemetryHTTPoison using the provided `opts` `Keyword list`.

  You should call this function within your application startup, before OpentelemetryHTTPoison is used.
  Using the `:ot_attributes` option, you can set default Open Telemetry metadata attributes
  to be added to each OpentelemetryHTTPoison request in the format of a list of two element tuples, with both elements
  being strings.

  Attributes can be overridden per each call to `OpentelemetryHTTPoison.request/1`.

  Using the `:infer_route` option, you can customise the URL resource route inference procedure
  that is used to set the `http.route` Open Telemetry metadata attribute.

  If a function with an arity of 1 (the `t:HTTPoison.Request/0` `request`) is provided
  then that function is used to determine the inference.

  If no value is provided then the out of the box, conservative inference provided by
  `OpentelemetryHTTPoison.URI.infer_route_from_request/1` is used to determine the inference.

  This can be overridden per each call to `OpentelemetryHTTPoison.request/1`.

  ## Examples
  iex> OpentelemetryHTTPoison.setup()
  :ok
  iex> infer_fn = fn
  ...>  %HTTPoison.Request{} = request -> URI.parse(request.url).path
  ...> end
  iex> OpentelemetryHTTPoison.setup(infer_route: infer_fn)
  :ok
  iex> OpentelemetryHTTPoison.setup(ot_attributes: [{"service.name", "..."}, {"service.namespace", "..."}])
  :ok
  iex> infer_fn = fn
  ...>  %HTTPoison.Request{} = request -> URI.parse(request.url).path
  ...> end
  iex> ot_attributes = [{"service.name", "..."}, {"service.namespace", "..."}]
  iex> OpentelemetryHTTPoison.setup(infer_route: infer_fn, ot_attributes: ot_attributes)
  :ok
  """
  def setup(opts \\ []) do
    Logger.warning("setup/1 is deprecated, use `config :opentelemetry_httpoison, ...` instead")

    Configuration.setup(opts)

    :ok
  end

  def process_request_headers(headers) when is_map(headers) do
    headers
    |> Enum.into([])
    |> process_request_headers()
  end

  def process_request_headers(headers) when is_list(headers) do
    headers
    # Convert atom header keys.
    # otel_propagator_text_map only accepts string keys, while Request.headers() keys can be atoms or strings.
    # The value in Request.headers() has to be a binary() so we don't need to convert it
    #
    # Note that this causes the header keys from HTTPoison.Response{request: %{headers: headers}} to also become strings
    # while with plain HTTPoison they would remain atoms.
    |> Enum.map(fn {k, v} -> {to_string(k), v} end)
    |> :otel_propagator_text_map.inject()
  end

  @doc ~S"""
  Performs a request using OpentelemetryHTTPoison with the provided `t:HTTPoison.Request/0` `request`.

  Depending how `OpentelemetryHTTPoison` is configured and whether or not the `:ot_resource_route`
  option is set to `:infer` (provided as a part of the `t:HTTPoison.Request/0` `options` `Keyword list`)
  this may attempt to automatically set the `http.route` Open Telemetry metadata attribute by obtaining
  the first segment of the `t:HTTPoison.Request/0` `url` (since this part typically does not contain dynamic data)

  If this behavior is not desirable, it can be set directly as a string or a function
  with an arity of 1 (the `t:HTTPoison.Request/0` `request`) by using the aforementioned `:ot_resource_route` option.

  It can also be circumvented entirely by supplying `:ignore`  instead.

    ## Examples

      iex> request = %HTTPoison.Request{
      ...> method: :post,
      ...> url: "https://www.example.com/users/edit/2",
      ...> body: ~s({"foo": 3}),
      ...> headers: [{"Accept", "application/json"}]}
      iex> OpentelemetryHTTPoison.request(request)

      iex> request = %HTTPoison.Request{
      ...> method: :post,
      ...> url: "https://www.example.com/users/edit/2",
      ...> body: ~s({"foo": 3}),
      ...> headers: [{"Accept", "application/json"}],
      ...> options: [ot_resource_route: :infer]}
      iex> OpentelemetryHTTPoison.request(request)

      iex> resource_route = "/users/edit/"
      iex> request = %HTTPoison.Request{
      ...> method: :post,
      ...> url: "https://www.example.com/users/edit/2",
      ...> body: ~s({"foo": 3}),
      ...> headers: [{"Accept", "application/json"}],
      ...> options: [ot_resource_route: resource_route]}
      iex> OpentelemetryHTTPoison.request(request)

      iex> infer_fn = fn
      ...>  %HTTPoison.Request{} = request -> URI.parse(request.url).path
      ...> end
      iex> request = %HTTPoison.Request{
      ...> method: :post,
      ...> url: "https://www.example.com/users/edit/2",
      ...> body: ~s({"foo": 3}),
      ...> headers: [{"Accept", "application/json"}],
      ...> options: [ot_resource_route: infer_fn]}
      iex> OpentelemetryHTTPoison.request(request)

      iex> request = %HTTPoison.Request{
      ...> method: :post,
      ...> url: "https://www.example.com/users/edit/2",
      ...> body: ~s({"foo": 3}),
      ...> headers: [{"Accept", "application/json"}],
      ...> options: [ot_resource_route: :ignore]}
      iex> OpentelemetryHTTPoison.request(request)

  """
  def request(%Request{options: opts} = request) do
    save_parent_ctx()

    span_name = Keyword.get_lazy(opts, :ot_span_name, fn -> default_span_name(request) end)

    uri = request.url |> process_request_url() |> URI.parse()

    resource_route_attribute =
      opts
      |> Keyword.get(:ot_resource_route, :unset)
      |> get_resource_route(request)
      |> case do
        resource_route when is_binary(resource_route) ->
          %{URLAttributes.url_template() => resource_route}

        nil ->
          %{}
      end

    ot_attributes =
      get_standard_ot_attributes(request, uri)
      |> Map.merge(get_ot_attributes(opts))
      |> Map.merge(resource_route_attribute)
      |> Enum.into([])

    request_ctx = Tracer.start_span(span_name, %{kind: :client, attributes: ot_attributes})
    Tracer.set_current_span(request_ctx)

    result = super(request)

    if Tracer.current_span_ctx() == request_ctx do
      case result do
        {:error, %{reason: reason}} ->
          Tracer.set_status(:error, inspect(reason))
          Tracer.set_attribute(ErrorAttributes.error_type(), format_error(reason))
          end_span()

        _ ->
          :ok
      end
    end

    result
  end

  def process_response_status_code(status_code) do
    # https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/semantic_conventions/http.md#status
    if status_code >= 400 do
      Tracer.set_status(:error, "")
      Tracer.set_attribute(ErrorAttributes.error_type(), Integer.to_string(status_code))
    end

    Tracer.set_attribute(HTTPAttributes.http_response_status_code(), status_code)
    end_span()
    status_code
  end

  defp end_span do
    Tracer.end_span()
    restore_parent_ctx()
  end

  # see https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/http.md#name
  defp default_span_name(request), do: request.method |> Atom.to_string() |> String.upcase()

  @ctx_key {__MODULE__, :parent_ctx}
  defp save_parent_ctx do
    ctx = Tracer.current_span_ctx()
    Process.put(@ctx_key, ctx)
  end

  defp restore_parent_ctx do
    ctx = Process.get(@ctx_key, :undefined)
    Process.delete(@ctx_key)
    Tracer.set_current_span(ctx)
  end

  defp get_standard_ot_attributes(request, uri) do
    %{
      HTTPAttributes.http_request_method() => http_method(request.method),
      ServerAttributes.server_address() => uri.host,
      ServerAttributes.server_port() => extract_port(uri),
      URLAttributes.url_full() => strip_uri_credentials(uri)
    }
  end

  defp http_method(method) do
    method
    |> Atom.to_string()
    |> String.upcase()
  end

  defp extract_port(%URI{port: port}) when is_integer(port), do: port

  defp extract_port(%URI{scheme: "https"}), do: 443

  defp extract_port(%URI{scheme: "http"}), do: 80

  defp extract_port(_), do: 80

  defp format_error(reason) when is_atom(reason), do: Atom.to_string(reason)
  defp format_error(reason), do: inspect(reason)

  defp get_ot_attributes(opts) do
    default_ot_attributes = Configuration.get(:ot_attributes)

    default_ot_attributes
    |> Enum.concat(Keyword.get(opts, :ot_attributes, []))
    |> Enum.reduce(%{}, fn {key, value}, acc -> Map.put(acc, key, value) end)
  end

  defp get_resource_route(option, request)

  defp get_resource_route(route, _) when is_binary(route), do: route

  defp get_resource_route(infer_fn, request) when is_function(infer_fn, 1), do: infer_fn.(request)

  defp get_resource_route(:infer, request), do: Configuration.get(:infer_route).(request)

  defp get_resource_route(:ignore, _), do: nil

  defp get_resource_route(:unset, _), do: nil

  defp get_resource_route(_unknown_option, _),
    do:
      raise(
        ArgumentError,
        "The :ot_resource_route keyword option value must either be a binary, a function with an arity of 1 or the :infer or :ignore atom"
      )

  defp strip_uri_credentials(%URI{} = uri) do
    uri |> Map.put(:userinfo, nil) |> Map.put(:authority, nil) |> URI.to_string()
  end

  defp strip_uri_credentials(url) when is_binary(url) do
    url |> URI.parse() |> strip_uri_credentials()
  end
end
