defmodule OpentelemetryProcessPropagator.TaskTest do
  use ExUnit.Case

  alias OpentelemetryProcessPropagator.Task

  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :application.start(:opentelemetry)

    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())

    :ok
  end

  defmodule OtherAppModule do
    def fun_with_span(value) do
      Tracer.with_span "already traced fun", %{attributes: %{value: value}} do
        value
      end
    end
  end

  describe "async_stream" do
    test "async_stream_with_ctx/3" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_ctx(["a", "b"], fn value ->
          OtherAppModule.fun_with_span(value)
        end)
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{} = attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      %{value: val1} = attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      %{value: val2} = attributes(attrs)

      assert ["a", "b"] == Enum.sort([val1, val2])
    end

    test "async_stream_with_ctx/5" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_ctx(["a", "b"], __MODULE__, :stream_ctx_test_function, [
          :async_stream_with_ctx
        ])
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{} = attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      %{value: val1} = attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      %{value: val2} = attributes(attrs)

      assert ["a", "b"] == Enum.sort([val1, val2])
    end

    test "async_stream_with_span/5" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_span(["a", "b"], "task span", %{attributes: %{a: 1}}, fn value ->
          Tracer.set_attribute(:value, value)

          value
        end)
        |> Stream.run()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
      assert %{value: val1, a: 1} = attributes(attrs)

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
      assert %{value: val2, a: 1} = attributes(attrs)

      assert ["a", "b"] == Enum.sort([val1, val2])
    end

    test "async_stream_with_span/7" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_span(
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          __MODULE__,
          :stream_test_function,
          [:async_stream_with_span]
        )
        |> Stream.run()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
      assert %{value: val1, a: 1} = attributes(attrs)

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
      assert %{value: val2, a: 1} = attributes(attrs)

      assert ["a", "b"] == Enum.sort([val1, val2])
    end

    test "async_stream_with_linked_span/5" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_linked_span(["a", "b"], "task span", %{attributes: %{a: 1}}, fn value ->
          Tracer.set_attribute(:value, value)

          :ok
        end)
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
      assert %{value: val1, a: 1} = attributes(attrs)
      assert [] != links

      assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
      assert %{value: val2, a: 1} = attributes(attrs)
      assert [] != links

      assert ["a", "b"] == Enum.sort([val1, val2])
    end

    test "async_stream_with_linked_span/7" do
      Tracer.with_span "parent span" do
        Task.async_stream_with_linked_span(
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          __MODULE__,
          :stream_test_function,
          [:async_stream_with_linked_span]
        )
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
      assert %{a: 1, value: val1} = attributes(attrs)
      assert [] != links

      assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
      assert %{a: 1, value: val2} = attributes(attrs)
      assert [] != links

      assert ["a", "b"] == Enum.sort([val1, val2])
    end
  end

  describe "async" do
    test "async_with_ctx/1" do
      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.async_with_ctx(fn ->
          Tracer.set_attribute(:value, :async_with_ctx)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{a: 1, value: :async_with_ctx} == attributes(attrs)
    end

    test "async_with_ctx/3" do
      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.async_with_ctx(__MODULE__, :ctx_test_function, [:async_with_ctx])
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "already traced fun", attributes: attrs)}
      assert %{value: :async_with_ctx} == attributes(attrs)
    end

    test "async_with_span/3" do
      Tracer.with_span "parent span" do
        Task.async_with_span("task span", %{attributes: %{a: 1}}, fn ->
          Tracer.set_attribute(:value, :async_with_span)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_with_span} == attributes(attrs)
    end

    test "async_with_span/5" do
      Tracer.with_span "parent span" do
        result =
          Task.async_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:async_with_span])
          |> Task.await()

        assert :async_with_span == result
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_with_span} == attributes(attrs)
    end

    test "async_with_linked_span/3" do
      Tracer.with_span "parent span" do
        Task.async_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
          Tracer.set_attribute(:value, :async_with_linked_span)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :async_with_linked_span} == attributes(attrs)
      assert [] != links
    end

    test "async_with_linked_span/6" do
      Tracer.with_span "parent span" do
        result =
          Task.async_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
            :async_with_linked_span
          ])
          |> Task.await()

        assert :async_with_linked_span == result
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :async_with_linked_span} == attributes(attrs)
      assert [] != links
    end
  end

  describe "start" do
    test "start_with_ctx/3" do
      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.start_with_ctx(fn ->
          ctx_test_function(:start_with_ctx)
        end)
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      assert %{value: :start_with_ctx} == attributes(attrs)
    end

    test "start_with_ctx/5" do
      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.start_with_ctx(__MODULE__, :ctx_test_function, [:start_with_ctx])
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      assert %{value: :start_with_ctx} == attributes(attrs)
    end

    test "start_with_span/3" do
      Tracer.with_span "parent span" do
        Task.start_with_span("task span", %{attributes: %{a: 1}}, fn ->
          Tracer.set_attribute(:value, :start_with_span)

          :ok
        end)
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "task span")}
      assert %{a: 1, value: :start_with_span} == attributes(attrs)
    end

    test "start_with_span/5" do
      Tracer.with_span "parent span" do
        Task.start_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:start_with_span])
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :start_with_span} == attributes(attrs)
    end

    test "start_with_linked_span" do
      Tracer.with_span "parent span" do
        Task.start_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
          Tracer.set_attribute(:value, :start_with_linked_span)

          :ok
        end)
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, attributes: attrs, links: links, name: "linked task span")}

      assert %{a: 1, value: :start_with_linked_span} == attributes(attrs)
      assert [] != links
    end

    test "start_with_linked_span_mfa" do
      Tracer.with_span "parent span" do
        Task.start_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
          :start_with_linked_span
        ])
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :start_with_linked_span} == attributes(attrs)
      assert [] != links
    end
  end

  test "start_link_with_ctx" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_link_with_ctx(fn ->
        ctx_test_function(:start_link_with_ctx)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
    assert %{a: 1} == attributes(attrs)

    assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
    assert %{value: :start_link_with_ctx} == attributes(attrs)
  end

  test "start_link_with_ctx_mfa" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_link_with_ctx(__MODULE__, :ctx_test_function, [:start_link_with_ctx])
    end

    assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
    assert %{a: 1} == attributes(attrs)

    assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "already traced fun")}
    assert %{value: :start_link_with_ctx} == attributes(attrs)
  end

  test "start_link_with_span" do
    Tracer.with_span "parent span" do
      Task.start_link_with_span("task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:value, :start_link_with_span)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span", span_id: root_span_id)}
    assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "task span")}

    assert %{a: 1, value: :start_link_with_span} == attributes(attrs)
  end

  test "start_link_with_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_link_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:start_link_with_span])
    end

    assert_receive {:span, span(name: "parent span", span_id: root_span_id)}
    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

    assert %{a: 1, value: :start_link_with_span} == attributes(attrs)
  end

  test "start_link_with_linked_span" do
    Tracer.with_span "parent span" do
      Task.start_link_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:value, :start_link_with_linked_span)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, attributes: attrs, links: links, name: "linked task span")}

    assert %{a: 1, value: :start_link_with_linked_span} == attributes(attrs)
    assert [] != links
  end

  test "start_link_with_linked_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_link_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
        :start_link_with_linked_span
      ])
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

    assert %{a: 1, value: :start_link_with_linked_span} == attributes(attrs)
    assert [] != links
  end

  def test_function(value) do
    Tracer.set_attributes(%{value: value})

    value
  end

  def ctx_test_function(value) do
    OtherAppModule.fun_with_span(value)
  end

  def link_test_function(value) do
    OtherAppModule.fun_with_span(value)
  end

  def stream_ctx_test_function(value, _args) do
    OtherAppModule.fun_with_span(value)
  end

  def stream_test_function(value, _args) do
    Tracer.set_attributes(%{value: value})

    value
  end

  defp attributes({_, _, _, _, attributes}), do: attributes
end
