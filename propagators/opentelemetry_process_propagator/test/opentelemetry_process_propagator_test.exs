defmodule OpentelemetryProcessPropagatorTest do
  use ExUnit.Case
  import OpentelemetryProcessPropagator

  require OpenTelemetry.Tracer, as: Tracer
  alias OpenTelemetry.Ctx

  setup_all do
    Application.put_env(:opentelemetry, :processors,
      otel_batch_processor: %{scheduled_delay_ms: 1, exporter: [otel_exporter_pid: self()]}
    )
  end

  describe "fetch_ctx/1" do
    test "returns undefined if no context found" do
      assert :undefined == fetch_ctx(self())
    end

    test "returns the ctx if there is one" do
      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      ctx = Ctx.get_current()

      assert ctx == fetch_ctx(self())
    end
  end

  describe "fetch_parent_ctx/0" do
    test "will not find parent ctx when spawned by kernel" do
      ctx = Tracer.start_span("test")
      Tracer.set_current_span(ctx)

      pid = self()

      spawn(fn ->
        send(pid, fetch_parent_ctx())
      end)

      assert_receive :undefined
    end

    test "fetches the parent ctx when spawned by proclib" do
      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      ctx = Ctx.get_current()

      pid = self()

      :proc_lib.spawn(fn ->
        p_ctx = fetch_parent_ctx()
        send(pid, p_ctx)
      end)

      assert_receive ^ctx
    end

    test "fetches the parent ctx when parent is named" do
      Process.register(self(), TestParent)

      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      ctx = Ctx.get_current()

      pid = self()

      :proc_lib.spawn(fn ->
        p_ctx = fetch_parent_ctx()
        send(pid, p_ctx)
      end)

      assert_receive ^ctx
    end
  end

  describe "fetch_parent_ctx/1" do
    test "will not find parent ctx when parent beyond max depth" do
      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      pid = self()

      :proc_lib.spawn(fn ->
        :proc_lib.spawn(fn ->
          send(pid, fetch_parent_ctx())
        end)
      end)

      assert_receive :undefined
    end

    test "fetches the parent ctx when within max depth" do
      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      ctx = Ctx.get_current()

      pid = self()

      :proc_lib.spawn(fn ->
        :proc_lib.spawn(fn ->
          send(pid, fetch_parent_ctx(2))
        end)
      end)

      assert_receive ^ctx
    end

    test "fetches the first ctx found" do
      span_ctx1 = Tracer.start_span("parent")
      Tracer.set_current_span(span_ctx1)

      ctx = Ctx.get_current()
      pid = self()

      span_ctx2 = Tracer.start_span("child")

      :proc_lib.spawn(fn ->
        Ctx.attach(ctx)
        Tracer.set_current_span(span_ctx2)

        Ctx.get_current()

        :proc_lib.spawn(fn ->
          send(pid, fetch_parent_ctx(2))
        end)

        # keep proc alive
        Process.sleep(10)
      end)

      refute_receive ^ctx
    end
  end

  describe "fetch_parent_ctx/2" do
    test "works with other keys" do
      span_ctx = Tracer.start_span("test")
      Tracer.set_current_span(span_ctx)

      ctx = Ctx.get_current()

      pid = self()

      Task.async(fn ->
        send(pid, fetch_parent_ctx(1, :"$callers"))
      end)

      assert_receive ^ctx
    end
  end

  defmodule GenServerTest do
    use GenServer

    def init(_), do: {:ok, %{}}

    def handle_call(:call, {pid, _ref}, state) do
      send(pid, fetch_parent_ctx())

      {:reply, :ok, state}
    end
  end

  test "works with otp behaviours" do
    {:ok, pid} = GenServer.start_link(GenServerTest, [])

    span_ctx = Tracer.start_span("test")
    Tracer.set_current_span(span_ctx)

    ctx = Ctx.get_current()

    GenServer.call(pid, :call)

    assert_receive ^ctx
  end
end
