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

  test "async_stream_with_ctx" do
    Tracer.with_span "parent span" do
      Task.async_stream_with_ctx(["a", "b"], fn letter ->
        Tracer.set_attribute(:letter, letter)

        :ok
      end)
      |> Stream.run()
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}

    assert %{letter: "b"} == attributes(attrs)
  end

  test "async_stream_with_span" do
    Tracer.with_span "parent span" do
      Task.async_stream_with_span(["a", "b"], "task span", %{attributes: %{a: 1}}, fn letter ->
        Tracer.set_attribute(:letter, letter)

        :ok
      end)
      |> Stream.run()
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}, 500

    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
    assert %{letter: "a", a: 1} == attributes(attrs)

    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}, 500
    assert %{letter: "b", a: 1} == attributes(attrs)
  end

  test "async_stream_with_linked_span" do
    Tracer.with_span "parent span" do
      Task.async_stream_with_linked_span(["a", "b"], "task span", %{attributes: %{a: 1}}, fn letter ->
        Tracer.set_attribute(:letter, letter)

        :ok
      end)
      |> Stream.run()
    end

    assert_receive {:span, span(name: "parent span")}, 500

    assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
    assert %{letter: "a", a: 1} == attributes(attrs)
    assert [] != links

    assert_receive {:span, span(parent_span_id: :undefined, links: links, name: "task span", attributes: attrs)}, 500
    assert %{letter: "b", a: 1} == attributes(attrs)
    assert [] != links
  end

  test "async_with_ctx" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.async_with_ctx(fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
      |> Task.await()
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "async_with_ctx_mfa" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.async_with_ctx(__MODULE__, :test_function, [:async_with_ctx])
      |> Task.await()
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "async_with_span" do
    Tracer.with_span "parent span" do
      Task.async_with_span("task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
      |> Task.await()
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "async_with_span_mfa" do
    Tracer.with_span "parent span" do
      result =
        Task.async_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:async_with_span])
        |> Task.await()

      assert :async_with_span == result
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "async_with_linked_span" do
    Tracer.with_span "parent span" do
      Task.async_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
      |> Task.await()
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  test "async_with_linked_span_mfa" do
    Tracer.with_span "parent span" do
      result =
        Task.async_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
          :async_with_linked_span
        ])
        |> Task.await()

      assert :async_with_linked_span == result
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  test "start_with_ctx" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_with_ctx(fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1} == attributes(attrs)
  end

  test "start_with_ctx_mfa" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_with_ctx(__MODULE__, :test_function, [:start_with_ctx])
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1} == attributes(attrs)
  end

  test "start_with_span" do
    Tracer.with_span "parent span" do
      Task.start_with_span("task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "start_with_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:start_with_span])
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "start_with_linked_span" do
    Tracer.with_span "parent span" do
      Task.start_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, attributes: attrs, links: links, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  test "start_with_linked_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
        :start_with_linked_span
      ])
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  test "start_link_with_ctx" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_link_with_ctx(fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1} == attributes(attrs)
  end

  test "start_link_with_ctx_mfa" do
    Tracer.with_span "parent span", %{attributes: %{a: 1}} do
      Task.start_link_with_ctx(__MODULE__, :test_function, [:start_with_ctx])
    end

    assert_receive {:span, span(name: "parent span", attributes: attrs)}
    assert %{a: 1} == attributes(attrs)
  end

  test "start_link_with_span" do
    Tracer.with_span "parent span" do
      Task.start_link_with_span("task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "start_link_with_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_link_with_span("task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [:start_with_span])
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

    assert %{a: 1, b: 2} == attributes(attrs)
  end

  test "start_link_with_linked_span" do
    Tracer.with_span "parent span" do
      Task.start_link_with_linked_span("linked task span", %{attributes: %{a: 1}}, fn ->
        Tracer.set_attribute(:b, 2)

        :ok
      end)
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, attributes: attrs, links: links, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  test "start_link_with_linked_span_mfa" do
    Tracer.with_span "parent span" do
      Task.start_link_with_linked_span("linked task span", %{attributes: %{a: 1}}, __MODULE__, :test_function, [
        :start_with_linked_span
      ])
    end

    assert_receive {:span, span(name: "parent span")}
    assert_receive {:span, span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}
    assert %{a: 1, b: 2} == attributes(attrs)
    assert [] != links
  end

  def test_function(arg) do
    Tracer.set_attribute(:b, 2)

    arg
  end

  defp attributes({_, _, _, _, attributes}), do: attributes
end
