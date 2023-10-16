defmodule OpentelemetryReqTest do
  use ExUnit.Case, async: true
  doctest OpentelemetryReq
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecordp(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    req =
      Req.new()
      |> OpentelemetryReq.attach()

    {:ok, req: req}
  end

  test "span", %{req: req} do
    adapter = fn request ->
      assert URI.to_string(request.url) == "/users/3"
      {request, Req.Response.new(status: 204)}
    end

    resp =
      Req.get!(req,
        adapter: adapter,
        url: "/users/:id",
        path_params: [id: 3]
      )

    assert resp.status == 204
    assert_receive {:span, span(name: "/users/:id")}
    refute_receive _
  end

  test "propagate traces", %{req: req} do
    adapter = fn request ->
      assert [value] = Req.Request.get_header(request, "traceparent")
      assert byte_size(value) > 10
      {request, Req.Response.new(status: 204)}
    end

    resp =
      Req.get!(req,
        adapter: adapter,
        url: "/",
        no_path_params: true,
        propagate_trace_ctx: true
      )

    assert resp.status == 204
    assert_receive {:span, span()}
    refute_receive _
  end
end
