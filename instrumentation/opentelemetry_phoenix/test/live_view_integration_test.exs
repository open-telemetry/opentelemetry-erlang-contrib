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

defmodule OtelLiveViewTest.ResourceLive do
  use Phoenix.LiveView, log: false

  def mount(_params, _session, socket), do: {:ok, socket}

  def handle_params(_params, _uri, socket), do: {:noreply, socket}

  def render(assigns) do
    ~H"""
    <main>{live_render(@socket, OtelLiveViewTest.ChildLive, id: "child")}</main>
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
  end
end

defmodule OtelLiveViewTest.Endpoint do
  use Phoenix.Endpoint, otp_app: :opentelemetry_phoenix

  plug(Plug.Session,
    store: :cookie,
    key: "_otel_lv_key",
    signing_salt: "K5K5K5K5"
  )

  plug(OtelLiveViewTest.Router)
end

defmodule OpentelemetryPhoenix.LiveViewIntegrationTest do
  use ExUnit.Case, async: false

  import Phoenix.ConnTest

  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes

  @endpoint OtelLiveViewTest.Endpoint

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())
    start_supervised!(OtelLiveViewTest.Endpoint)

    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    on_exit(fn ->
      Enum.each(:telemetry.list_handlers([]), &:telemetry.detach(&1.id))
    end)

    :ok
  end

  test "a real LiveView dead render emits mount and handle_params spans carrying http.route" do
    conn = get(build_conn(), "/resources/123")

    assert html_response(conn, 200) =~ "child"

    spans = collect_spans()

    assert %{HTTPAttributes.http_route() => "/resources/:resource_id"} ==
             Map.fetch!(spans, "OtelLiveViewTest.ResourceLive.mount")

    assert %{HTTPAttributes.http_route() => "/resources/:resource_id"} ==
             Map.fetch!(spans, "OtelLiveViewTest.ResourceLive.handle_params")
  end

  test "a nested LiveView not mounted at the router is traced without a route" do
    conn = get(build_conn(), "/resources/123")

    assert html_response(conn, 200) =~ "child"

    spans = collect_spans()

    assert %{} == Map.fetch!(spans, "OtelLiveViewTest.ChildLive.mount")

    assert [_] = :telemetry.list_handlers([:phoenix, :live_view, :mount, :start])
  end

  defp collect_spans(acc \\ %{}) do
    receive do
      {:span, span(name: name, attributes: attributes)} ->
        collect_spans(Map.put_new(acc, name, :otel_attributes.map(attributes)))
    after
      200 -> acc
    end
  end
end
