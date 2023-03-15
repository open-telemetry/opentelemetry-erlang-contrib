defmodule OpentelemetryReq do
  @moduledoc """
  Wraps the request in an opentelemetry span. Span names must be paramaterized, so the
  `req_path_params` module and step should be registered before this step. This step is
  expected by default and an error will be raised if the path params option is
  not set for the request.

  Given the steps pipeline can be halted to skip further steps from running, it is important
  to _append_ request and response steps _after_ this step to ensure execution. Spans are not
  created until the request is completed or errored.

  ### Example

  ```
  client =
    Req.new()
    |> OpentelemetryReq.attach()
    |> Req.Request.merge_options(
      base_url: "http://localhost:4000",
      propagate_trace_ctx: true
    )

  client
  |> Req.get(
    url: "/api/users/:user_id",
    path_params: [user_id: user_id]
  )
  ```
  ## Request Options

    * `:span_name` - `String.t()` if provided, overrides the span name. Defaults to `nil`.
    * `:no_path_params` - `boolean()` when set to `true` no path params are expected for the request. Defaults to `false`
    * `:propagate_trace_ctx` - `boolean()` when set to `true`, trace headers will be propagated. Defaults to `false`
  """

  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SemanticConventions.Trace
  require Trace
  require Tracer
  require Logger

  def attach(%Req.Request{} = request, options \\ []) do
    request
    |> Req.Request.register_options([:span_name, :no_path_params, :propagate_trace_ctx])
    |> Req.Request.merge_options(options)
    |> Req.Request.append_request_steps(
      require_path_params: &require_path_params_option/1,
      start_span: &start_span/1,
      put_trace_headers: &maybe_put_trace_headers/1
    )
    |> Req.Request.prepend_response_steps(otel_end_span: &end_span/1)
    |> Req.Request.prepend_error_steps(otel_end_span: &end_errored_span/1)
  end

  defp start_span(request) do
    span_name = span_name(request)

    attrs = build_req_attrs(request)

    Tracer.start_span(span_name, %{
      attributes: attrs,
      kind: :client
    })
    |> Tracer.set_current_span()

    request
  end

  defp end_span({request, %Req.Response{} = response}) do
    attrs =
      Map.put(%{}, Trace.http_status_code(), response.status)
      |> maybe_append_resp_content_length(response)

    Tracer.set_attributes(attrs)

    if response.status >= 400 do
      OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, ""))
    end

    OpenTelemetry.Tracer.end_span()

    {request, response}
  end

  defp end_errored_span({request, exception}) do
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, format_exception(exception)))

    OpenTelemetry.Tracer.end_span()

    {request, exception}
  end

  defp format_exception(%{__exception__: true} = exception) do
    Exception.message(exception)
  end

  defp format_exception(_), do: ""

  defp span_name(request) do
    Req.Request.get_private(request, :path_params_template)
  end

  defp build_req_attrs(request) do
    uri = request.url
    url = url(uri)

    %{
      Trace.http_method() => http_method(request.method),
      Trace.http_url() => url,
      Trace.http_target() => uri.path,
      Trace.net_host_name() => uri.host,
      Trace.http_scheme() => uri.scheme
    }
    |> maybe_append_req_content_length(request)
    |> maybe_append_retry_count(request)
  end

  defp maybe_append_req_content_length(attrs, req) do
    case Req.Request.get_header(req, "content-length") do
      [] ->
        attrs

      [length] ->
        Map.put(attrs, Trace.http_request_content_length(), length)
    end
  end

  defp maybe_append_resp_content_length(attrs, req) do
    case Req.Response.get_header(req, "content-length") do
      [] ->
        attrs

      [length] ->
        Map.put(attrs, Trace.http_response_content_length(), length)
    end
  end

  defp maybe_append_retry_count(attrs, req) do
    retry_count = Req.Request.get_private(req, :req_retry_count, 0)

    if retry_count > 0 do
      Map.put(attrs, Trace.http_retry_count(), retry_count)
    else
      attrs
    end
  end

  defp url(uri) do
    if uri.query do
      uri.path <> "?" <> uri.query
    else
      uri.path
    end
  end

  defp http_method(method) do
    case method do
      :get -> :GET
      :head -> :HEAD
      :post -> :POST
      :patch -> :PATCH
      :put -> :PUT
      :delete -> :DELETE
      :connect -> :CONNECT
      :options -> :OPTIONS
      :trace -> :TRACE
    end
  end

  defp maybe_put_trace_headers(request) do
    if request.options[:propagate_trace_ctx] do
      Map.put(request, :headers, :otel_propagator_text_map.inject(request.headers))
    else
      request
    end
  end

  defp require_path_params_option(request) do
    unless request.options[:no_path_params] do
      unless Map.has_key?(request.options, :path_params) do
        {Req.Request.halt(request), __MODULE__.PathParamsOptionError.new()}
      else
        request
      end
    else
      request
    end
  end

  defmodule PathParamsOptionError do
    defexception [:message]

    def new do
      %__MODULE__{}
    end

    @impl true
    def message(_) do
      "req_path_params path parameter options must be provided"
    end
  end
end
