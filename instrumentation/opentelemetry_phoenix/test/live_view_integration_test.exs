Application.put_env(:opentelemetry_phoenix, OtelLiveViewTest.Endpoint,
  secret_key_base: String.duplicate("abcdefgh", 8),
  live_view: [signing_salt: "K5K5K5K5"],
  render_errors: [formats: [html: OtelLiveViewTest.ErrorHTML]]
)

defmodule OtelLiveViewTest.ErrorHTML do
  def render(template, _assigns), do: template
end

defmodule OtelLiveViewTest.ChildLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, socket}

  def render(assigns), do: ~H"<span>child</span>"
end

defmodule OtelLiveViewTest.CounterComponent do
  use Phoenix.LiveComponent

  def update(assigns, socket) do
    {:ok, socket |> assign(assigns) |> assign_new(:bumps, fn -> 0 end)}
  end

  def handle_event("bump", _params, socket) do
    {:noreply, assign(socket, :bumps, socket.assigns.bumps + 1)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <button phx-click="bump" phx-target={@myself}>bumped {@bumps}</button>
    </div>
    """
  end
end

defmodule OtelLiveViewTest.ResourceLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, assign(socket, :clicks, 0)}

  def handle_params(_params, _uri, socket), do: {:noreply, socket}

  def handle_event("hello", _params, socket) do
    {:noreply, assign(socket, :clicks, socket.assigns.clicks + 1)}
  end

  def handle_event("boom", _params, _socket), do: raise("handle_event error")

  def render(assigns) do
    ~H"""
    <main>
      {live_render(@socket, OtelLiveViewTest.ChildLive, id: "child")}
      <.live_component module={OtelLiveViewTest.CounterComponent} id="counter" />
      <button phx-click="hello">clicked {@clicks}</button>
      <button phx-click="boom">boom</button>
    </main>
    """
  end
end

defmodule OtelLiveViewTest.MountErrorLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, _socket), do: raise("mount error")

  def render(assigns), do: ~H"<span>unreachable</span>"
end

defmodule OtelLiveViewTest.ParamsErrorLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, socket}

  def handle_params(_params, _uri, _socket), do: raise("handle_params error")

  def render(assigns), do: ~H"<span>unreachable</span>"
end

defmodule OtelLiveViewTest.RenderErrorLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, assign(socket, :explode, false)}

  def handle_event("explode", _params, socket), do: {:noreply, assign(socket, :explode, true)}

  def render(%{explode: true}), do: raise("render error")

  def render(assigns) do
    ~H"""
    <button phx-click="explode">explode</button>
    """
  end
end

defmodule OtelLiveViewTest.UpdateErrorComponent do
  use Phoenix.LiveComponent

  def update(_assigns, _socket), do: raise("update error")

  def render(assigns), do: ~H"<span>unreachable</span>"
end

defmodule OtelLiveViewTest.UpdateErrorLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, socket}

  def render(assigns) do
    ~H"""
    <.live_component module={OtelLiveViewTest.UpdateErrorComponent} id="broken" />
    """
  end
end

defmodule OtelLiveViewTest.Router do
  use Phoenix.Router, helpers: false

  import Phoenix.LiveView.Router

  pipeline :browser do
    plug(:fetch_session)
  end

  scope "/" do
    pipe_through(:browser)

    live("/resources/:resource_id", OtelLiveViewTest.ResourceLive, :show)
    live("/mount-error", OtelLiveViewTest.MountErrorLive)
    live("/params-error", OtelLiveViewTest.ParamsErrorLive)
    live("/render-error", OtelLiveViewTest.RenderErrorLive)
    live("/update-error", OtelLiveViewTest.UpdateErrorLive)
  end
end

defmodule OtelLiveViewTest.Endpoint do
  use Phoenix.Endpoint, otp_app: :opentelemetry_phoenix

  @session_options [
    store: :cookie,
    key: "_otel_lv_key",
    signing_salt: "K5K5K5K5"
  ]

  socket("/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]])

  plug(Plug.Session, @session_options)

  plug(OtelLiveViewTest.Router)
end

defmodule OpentelemetryPhoenix.LiveViewIntegrationTest do
  use ExUnit.Case, async: false

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ExceptionAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes

  @moduletag :capture_log

  @endpoint OtelLiveViewTest.Endpoint
  @route "/resources/:resource_id"

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup context do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())
    start_supervised!(OtelLiveViewTest.Endpoint)

    OpentelemetryPhoenix.setup([adapter: :cowboy2] ++ Map.get(context, :setup_opts, []))

    on_exit(fn ->
      Enum.each(
        [
          {OpentelemetryPhoenix, :endpoint_start},
          {OpentelemetryPhoenix, :router_dispatch_start},
          {OpentelemetryPhoenix, :live_view},
          {OpentelemetryPhoenix, :controller_render}
        ],
        &:telemetry.detach/1
      )
    end)

    :ok
  end

  describe "dead render" do
    test "emits mount and handle_params spans carrying http.route" do
      conn = get(build_conn(), "/resources/123")

      assert html_response(conn, 200) =~ "child"

      spans = collect_spans()

      assert %{HTTPAttributes.http_route() => @route} ==
               attributes(spans, "OtelLiveViewTest.ResourceLive.mount")

      assert %{HTTPAttributes.http_route() => @route} ==
               attributes(spans, "OtelLiveViewTest.ResourceLive.handle_params")
    end

    test "traces a nested LiveView not mounted at the router without a route" do
      conn = get(build_conn(), "/resources/123")

      assert html_response(conn, 200) =~ "child"

      assert %{} == attributes(collect_spans(), "OtelLiveViewTest.ChildLive.mount")
    end

    test "emits a live_component update span attributed to the parent LiveView" do
      get(build_conn(), "/resources/123")

      assert %{:"live_view.module" => "OtelLiveViewTest.ResourceLive"} ==
               attributes(collect_spans(), "OtelLiveViewTest.CounterComponent.update")
    end
  end

  describe "connected mount" do
    test "emits a render span for the LiveView" do
      {:ok, _view, _html} = live(build_conn(), "/resources/123")

      assert %{} == attributes(collect_spans(), "OtelLiveViewTest.ResourceLive.render")
    end

    test "emits a handle_event span" do
      {:ok, view, _html} = live(build_conn(), "/resources/123")

      drain_spans()

      render_click(view, "hello", %{})

      assert %{} ==
               attributes(collect_spans(), "OtelLiveViewTest.ResourceLive.handle_event#hello")
    end

    test "emits a live_component handle_event span" do
      {:ok, view, _html} = live(build_conn(), "/resources/123")

      drain_spans()

      view |> element("button[phx-click=bump]") |> render_click()

      spans = collect_spans()

      assert %{} == attributes(spans, "OtelLiveViewTest.ResourceLive.handle_event#bump")

      assert %{:"live_view.module" => "OtelLiveViewTest.ResourceLive"} ==
               attributes(spans, "OtelLiveViewTest.CounterComponent.render")
    end
  end

  describe "exceptions" do
    test "records an exception on the mount span" do
      crash(fn -> get(build_conn(), "/mount-error") end)

      assert_exception_recorded(collect_spans(), "OtelLiveViewTest.MountErrorLive.mount")
    end

    test "records an exception on the handle_params span" do
      crash(fn -> get(build_conn(), "/params-error") end)

      assert_exception_recorded(
        collect_spans(),
        "OtelLiveViewTest.ParamsErrorLive.handle_params"
      )
    end

    test "records an exception on the live_component update span" do
      crash(fn -> get(build_conn(), "/update-error") end)

      assert_exception_recorded(
        collect_spans(),
        "OtelLiveViewTest.UpdateErrorComponent.update"
      )
    end

    test "records an exception on the handle_event span" do
      {:ok, view, _html} = live(build_conn(), "/resources/123")

      drain_spans()

      crash(fn -> render_click(view, "boom", %{}) end)

      assert_exception_recorded(
        collect_spans(),
        "OtelLiveViewTest.ResourceLive.handle_event#boom"
      )
    end

    test "records an exception on the render span" do
      {:ok, view, _html} = live(build_conn(), "/render-error")

      drain_spans()

      crash(fn -> render_click(view, "explode", %{}) end)

      assert_exception_recorded(collect_spans(), "OtelLiveViewTest.RenderErrorLive.render")
    end
  end

  describe "liveview: false" do
    @tag setup_opts: [liveview: false]
    test "records no LiveView spans" do
      {:ok, view, _html} = live(build_conn(), "/resources/123")

      render_click(view, "hello", %{})

      refute Enum.any?(Map.keys(collect_spans()), &String.contains?(&1, "OtelLiveViewTest"))
    end
  end

  defp crash(fun) do
    Process.flag(:trap_exit, true)

    try do
      fun.()
    catch
      _kind, _reason -> :ok
    end
  end

  defp assert_exception_recorded(spans, name) do
    span(status: status, events: events) = Map.fetch!(spans, name)

    assert status(code: :error) = status

    [event(name: :exception, attributes: event_attributes)] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] == Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  defp attributes(spans, name) do
    span(attributes: attributes) = Map.fetch!(spans, name)
    :otel_attributes.map(attributes)
  end

  defp drain_spans do
    collect_spans()
    :ok
  end

  defp collect_spans(acc \\ %{}) do
    receive do
      {:span, span(name: name) = recorded} -> collect_spans(Map.put_new(acc, name, recorded))
    after
      200 -> acc
    end
  end
end
