defmodule OpentelemetryPhoenix.LiveViewTest do
  defmodule ErrorHTML do
    def render(template, _assigns) do
      Phoenix.Controller.status_message_from_template(template)
    end
  end

  defmodule TestLive do
    use Phoenix.LiveView, layout: false

    require OpenTelemetry.Tracer
    require OpentelemetryPhoenix.LiveView

    @impl true
    def mount(_params, _session, socket) do
      {:ok, socket}
    end

    @impl true
    def handle_params(_params, _url, socket) do
      socket =
        OpenTelemetry.Tracer.with_span "parent span" do
          socket
          |> OpentelemetryPhoenix.LiveView.assign_async(:assign_async, fn ->
            OpenTelemetry.Tracer.with_span "assign_async span" do
              {:ok, %{assign_async: "assign_async.loaded"}}
            end
          end)
          |> OpentelemetryPhoenix.LiveView.start_async(:start_async, fn ->
            OpenTelemetry.Tracer.with_span "start_async span" do
              "start_async.loaded"
            end
          end)
        end

      {:noreply, socket}
    end

    @impl true
    def handle_async(:start_async, {:ok, value}, socket) do
      {:noreply, assign(socket, :start_async, Phoenix.LiveView.AsyncResult.ok(value))}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <%= @assign_async.ok? && @assign_async.result %>
      <%= assigns[:start_async] && @start_async.ok? && @start_async.result %>
      """
    end
  end

  defmodule Router do
    use Phoenix.Router, helpers: false

    import Phoenix.LiveView.Router

    live "/test", TestLive, :show
  end

  defmodule Endpoint do
    use Phoenix.Endpoint, otp_app: :opentelemetry_phoenix

    plug(Router)
  end

  use ExUnit.Case, async: false

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  @endpoint Endpoint

  setup do
    Application.put_env(
      :opentelemetry_phoenix,
      Endpoint,
      [
        secret_key_base: "secret_key_base",
        live_view: [signing_salt: "signing_salt"],
        render_errors: [formats: [html: ErrorHTML]]
      ]
    )
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    {:ok, _} = start_supervised(Endpoint)

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  @tag capture_log: true
  test "render_async", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/test")

    assert html = render_async(view)
    assert html =~ "assign_async.loaded"
    assert html =~ "start_async.loaded"

    # Initial parent span from the REST request
    assert_receive {:span, span(name: "parent span")}

    # Parent span from the socket
    assert_receive {:span,
                    span(
                      name: "parent span",
                      trace_id: trace_id,
                      span_id: process_span_id
                    )}

    assert_receive {:span,
                    span(
                      name: "assign_async span",
                      trace_id: ^trace_id,
                      parent_span_id: ^process_span_id
                    )}


    assert_receive {:span,
                    span(
                      name: "start_async span",
                      trace_id: ^trace_id,
                      parent_span_id: ^process_span_id
                    )}
  end
end
