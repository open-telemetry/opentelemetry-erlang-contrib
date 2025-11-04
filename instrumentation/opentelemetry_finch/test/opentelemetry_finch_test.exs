defmodule OpentelemetryFinchTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryFinch

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)

    {:ok, bypass: Bypass.open()}
  end

  test "records span on requests", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)
    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: HttpFinch})

    {:ok, _} = Finch.build(:get, endpoint_url(bypass.port)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "HTTP GET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "net.peer.name": "localhost",
             "http.method": "GET",
             "http.target": "/",
             "http.scheme": :http,
             "http.status_code": 200
           } = :otel_attributes.map(attributes)
  end

  test "records span on streamed requests where the acc is a 3 elements tuple", %{
    bypass: bypass,
    test: finch_name
  } do
    Bypass.expect_once(bypass, "GET", "/", fn conn ->
      Plug.Conn.send_resp(conn, 200, "OK")
    end)

    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: finch_name})

    acc = {nil, [], ""}

    fun = fn
      {:status, value}, {_, headers, body} -> {:cont, {value, headers, body}}
      {:headers, value}, {status, headers, body} -> {:cont, {status, headers ++ value, body}}
      {:data, value}, {status, headers, body} -> {:cont, {status, headers, body <> value}}
    end

    assert {:ok, {200, [_ | _], "OK"}} =
             Finch.build(:get, endpoint_url(bypass.port))
             |> Finch.stream_while(finch_name, acc, fun)

    assert_receive {:span,
                    span(
                      name: "HTTP GET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "net.peer.name": "localhost",
             "http.method": "GET",
             "http.target": "/",
             "http.scheme": :http,
             "http.status_code": 200
           } = :otel_attributes.map(attributes)
  end

  test "records span on streamed requests where the acc is a 2 elements tuple", %{
    bypass: bypass,
    test: finch_name
  } do
    Bypass.expect_once(bypass, "GET", "/", fn conn ->
      Plug.Conn.send_resp(conn, 200, "OK")
    end)

    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: finch_name})

    acc = {nil, %{body: "", headers: %{}, trailers: [], status: 200}}

    fun = fn
      {:status, status}, {req, resp} ->
        {:cont, {req, %{resp | status: status}}}

      {:headers, fields}, {req, resp} ->
        fields = finch_fields_to_map(fields)
        resp = update_in(resp.headers, &Map.merge(&1, fields))
        {:cont, {req, resp}}

      {:data, _data}, acc ->
        {:cont, acc}

      {:trailers, fields}, {req, resp} ->
        fields = finch_fields_to_map(fields)
        resp = update_in(resp.trailers, &Map.merge(&1, fields))
        {:cont, {req, resp}}
    end

    assert {:ok,
            {_request,
             %{
               status: 200,
               body: "",
               headers: %{
                 "cache-control" => _,
                 "content-length" => ["2"],
                 "date" => [_],
                 "server" => _
               },
               trailers: []
             }}} =
             Finch.build(:get, endpoint_url(bypass.port))
             |> Finch.stream_while(finch_name, acc, fun)

    assert_receive {:span,
                    span(
                      name: "HTTP GET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "net.peer.name": "localhost",
             "http.method": "GET",
             "http.target": "/",
             "http.scheme": :http,
             "http.status_code": 200
           } = :otel_attributes.map(attributes)
  end

  test "records span on requests failed", %{bypass: _} do
    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: HttpFinch})

    {:error, _} = Finch.build(:get, endpoint_url(3333)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "HTTP GET",
                      kind: :client,
                      status: {:status, :error, "connection refused"},
                      attributes: attributes
                    )}

    assert %{
             "net.peer.name": "localhost",
             "http.method": "GET",
             "http.target": "/",
             "http.scheme": :http,
             "http.status_code": 0
           } = :otel_attributes.map(attributes)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"

  defp finch_fields_to_map(fields) do
    Enum.reduce(fields, %{}, fn {name, value}, acc ->
      Map.update(acc, name, [value], &(&1 ++ [value]))
    end)
  end
end
