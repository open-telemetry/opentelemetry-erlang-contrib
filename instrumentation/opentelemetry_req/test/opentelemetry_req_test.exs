defmodule OpentelemetryReqTest do
  use ExUnit.Case, async: true

  doctest OpentelemetryReq

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    :ok
  end

  test "span name and attributes" do
    {:ok, _resp} =
      Req.new(adapter: fn req -> {req, Req.Response.new()} end)
      |> OpentelemetryReq.attach()
      |> Req.get(url: "http://example.com/users", span_name: "GET")

    assert_receive {:span, span(name: "GET", kind: :client, attributes: attributes)}

    assert %{
             "http.url": "http://example.com/users",
             "http.method": :GET,
             "http.scheme": "http",
             "http.target": "/users",
             "http.status_code": 200,
             "net.host.name": "example.com"
           } = :otel_attributes.map(attributes)
  end

  describe "provided span name" do
    test "skip path params validation" do
      {:ok, _resp} =
        Req.new(adapter: fn req -> {req, Req.Response.new()} end)
        |> OpentelemetryReq.attach()
        |> Req.get(url: "http://example.com/users", span_name: "provided")

      assert_receive {:span, span(name: "provided")}
    end
  end

  describe "inferred span name" do
    test "use method and path params template" do
      {:ok, _resp} =
        Req.new(adapter: fn req -> {req, Req.Response.new()} end)
        |> OpentelemetryReq.attach()
        |> Req.get(url: "http://example.com/users/:id", path_params: [id: 42])

      assert_receive {:span, span(name: "GET /users/:id")}
    end

    test "validate presence of path params" do
      assert {:error, exception} =
               Req.new(adapter: fn req -> {req, Req.Response.new()} end)
               |> OpentelemetryReq.attach()
               |> Req.get(url: "http://example.com/users/:id")

      assert Exception.message(exception) =~ ":path_params option must be set"
    end

    test "allow override absence of path params" do
      {:ok, _resp} =
        Req.new(adapter: fn req -> {req, Req.Response.new()} end)
        |> OpentelemetryReq.attach()
        |> Req.get(url: "http://example.com/users", no_path_params: true)

      assert_receive {:span, span(name: "GET /users")}
    end
  end

  describe "propagation" do
    test "does not propagate by default" do
      {:ok, _resp} =
        Req.new(adapter: echo_adapter(self()))
        |> OpentelemetryReq.attach()
        |> Req.get(
          url: "http://example.com/users",
          no_path_params: true
        )

      assert_receive {:request, req}
      assert [] = Req.Request.get_header(req, "traceparent")

      assert_receive {:span, span(name: _name)}
    end

    test "propagate headers if required" do
      {:ok, _resp} =
        Req.new(adapter: echo_adapter(self()))
        |> OpentelemetryReq.attach()
        |> Req.get(
          url: "http://example.com/users",
          no_path_params: true,
          propagate_trace_ctx: true
        )

      assert_receive {:request, req}
      assert [traceparent] = Req.Request.get_header(req, "traceparent")
      assert is_binary(traceparent)

      assert_receive {:span, span(name: _name)}
    end
  end

  defp echo_adapter(pid) do
    fn req ->
      send(pid, {:request, req})
      {req, Req.Response.new()}
    end
  end
end
