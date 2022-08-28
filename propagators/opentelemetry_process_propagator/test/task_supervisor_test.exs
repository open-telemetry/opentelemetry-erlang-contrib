defmodule OpentelemetryProcessPropagator.TaskSupervisorTest do
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
    test "async_stream" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream(Test.TaskSupervisor, ["a", "b"], fn value ->
          OtherAppModule.fun_with_span(value)
        end)
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id)}

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs1)}
      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs2)}

      assert [%{value: "a"}, %{value: "b"}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream/5" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream(Test.TaskSupervisor, ["a", "b"], __MODULE__, :stream_ctx_test_function, [
          :async_stream
        ])
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{} == attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", attributes: attrs1)}
      assert_receive {:span, span(name: "already traced fun", attributes: attrs2)}

      assert [%{value: "a"}, %{value: "b"}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream_with_span/6" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_with_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          fn value ->
            Tracer.set_attribute(:value, value)

            value
          end
        )
        |> Stream.run()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs1)}, 500
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs2)}, 500

      assert [%{value: "a", a: 1}, %{value: "b", a: 1}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream_with_span/8" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_with_span(
          Test.TaskSupervisor,
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

    test "async_stream_with_linked_span/6" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_with_linked_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          fn value ->
            Tracer.set_attribute(:value, value)

            :ok
          end
        )
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

    test "async_stream_with_linked_span/8" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_with_linked_span(
          Test.TaskSupervisor,
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

  describe "async_stream_nolink" do
    test "async_stream_nolink" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink(Test.TaskSupervisor, ["a", "b"], fn value ->
          OtherAppModule.fun_with_span(value)
        end)
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id)}

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs1)}
      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs2)}

      assert [%{value: "a"}, %{value: "b"}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream_nolink/5" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink(
          Test.TaskSupervisor,
          ["a", "b"],
          __MODULE__,
          :stream_ctx_test_function,
          [
            :async_stream_nolink
          ]
        )
        |> Stream.run()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{} == attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", attributes: attrs1)}
      assert_receive {:span, span(name: "already traced fun", attributes: attrs2)}

      assert [%{value: "a"}, %{value: "b"}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream_nolink_with_span/6" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink_with_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          fn value ->
            Tracer.set_attribute(:value, value)

            value
          end
        )
        |> Stream.run()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}, 500

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs1)}, 500
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs2)}, 500

      assert [%{value: "a", a: 1}, %{value: "b", a: 1}] == Enum.sort([attributes(attrs1), attributes(attrs2)])
    end

    test "async_stream_nolink_with_span/8" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink_with_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          __MODULE__,
          :stream_test_function,
          [:async_stream_nolink_with_span]
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

    test "async_stream_nolink_with_linked_span/6" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink_with_linked_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          fn value ->
            Tracer.set_attribute(:value, value)

            :ok
          end
        )
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

    test "async_stream_nolink_with_linked_span/8" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_stream_nolink_with_linked_span(
          Test.TaskSupervisor,
          ["a", "b"],
          "task span",
          %{attributes: %{a: 1}},
          __MODULE__,
          :stream_test_function,
          [:async_stream_nolink_with_linked_span]
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
    test "async" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.async(Test.TaskSupervisor, fn ->
          Tracer.set_attribute(:value, :async)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{a: 1, value: :async} == attributes(attrs)
    end

    test "async_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.async(Test.TaskSupervisor, __MODULE__, :test_function, [:async])
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{a: 1, value: :async} == attributes(attrs)
    end

    test "async_with_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_with_span("task span", %{attributes: %{a: 1}}, Test.TaskSupervisor, fn ->
          Tracer.set_attribute(:value, :async_with_span)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_with_span} == attributes(attrs)
    end

    test "async_with_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        result =
          Task.Supervisor.async_with_span(
            "task span",
            %{attributes: %{a: 1}},
            Test.TaskSupervisor,
            __MODULE__,
            :test_function,
            [:async_with_span]
          )
          |> Task.await()

        assert :async_with_span == result
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_with_span} == attributes(attrs)
    end

    test "async_with_linked_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_with_linked_span("linked task span", %{attributes: %{a: 1}}, Test.TaskSupervisor, fn ->
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

    test "async_with_linked_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        result =
          Task.Supervisor.async_with_linked_span(
            "linked task span",
            %{attributes: %{a: 1}},
            Test.TaskSupervisor,
            __MODULE__,
            :test_function,
            [
              :async_with_linked_span
            ]
          )
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

  describe "async_nolink" do
    test "async_nolink" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.async_nolink(Test.TaskSupervisor, fn ->
          Tracer.set_attribute(:value, :async_nolink)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", attributes: attrs)}
      assert %{a: 1, value: :async_nolink} == attributes(attrs)
    end

    test "async_nolink_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.async_nolink(Test.TaskSupervisor, __MODULE__, :ctx_test_function, [
          :async_nolink
        ])
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "already traced fun", attributes: attrs)}
      assert %{value: :async_nolink} == attributes(attrs)
    end

    test "async_nolink_with_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_nolink_with_span("task span", %{attributes: %{a: 1}}, Test.TaskSupervisor, fn ->
          Tracer.set_attribute(:value, :async_nolink_with_span)

          :ok
        end)
        |> Task.await()
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_nolink_with_span} == attributes(attrs)
    end

    test "async_nolink_with_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        result =
          Task.Supervisor.async_nolink_with_span(
            "task span",
            %{attributes: %{a: 1}},
            Test.TaskSupervisor,
            __MODULE__,
            :test_function,
            [:async_nolink_with_span]
          )
          |> Task.await()

        assert :async_nolink_with_span == result
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :async_nolink_with_span} == attributes(attrs)
    end

    test "async_nolink_with_linked_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.async_nolink_with_linked_span(
          "linked task span",
          %{attributes: %{a: 1}},
          Test.TaskSupervisor,
          fn ->
            Tracer.set_attribute(:value, :async_nolink_with_linked_span)

            :ok
          end
        )
        |> Task.await()
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :async_nolink_with_linked_span} == attributes(attrs)
      assert [] != links
    end

    test "async_nolink_with_linked_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        result =
          Task.Supervisor.async_nolink_with_linked_span(
            "linked task span",
            %{attributes: %{a: 1}},
            Test.TaskSupervisor,
            __MODULE__,
            :test_function,
            [
              :async_nolink_with_linked_span
            ]
          )
          |> Task.await()

        assert :async_nolink_with_linked_span == result
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :async_nolink_with_linked_span} == attributes(attrs)
      assert [] != links
    end
  end

  describe "start_child" do
    test "start_child" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.start_child(Test.TaskSupervisor, fn ->
          ctx_test_function(:start_child)
        end)
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(name: "already traced fun", parent_span_id: ^root_span_id, attributes: attrs)}
      assert %{value: :start_child} == attributes(attrs)
    end

    test "start_child_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span", %{attributes: %{a: 1}} do
        Task.Supervisor.start_child(Test.TaskSupervisor, __MODULE__, :ctx_test_function, [
          :start_child
        ])
      end

      assert_receive {:span, span(name: "parent span", span_id: root_span_id, attributes: attrs)}
      assert %{a: 1} == attributes(attrs)

      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "already traced fun", attributes: attrs)}
      assert %{value: :start_child} == attributes(attrs)
    end

    test "start_child_with_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.start_child_with_span("task span", %{attributes: %{a: 1}}, Test.TaskSupervisor, fn ->
          Tracer.set_attribute(:value, :start_child_with_span)

          :ok
        end)
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, attributes: attrs, name: "task span")}
      assert %{a: 1, value: :start_child_with_span} == attributes(attrs)
    end

    test "start_child_with_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.start_child_with_span(
          "task span",
          %{attributes: %{a: 1}},
          Test.TaskSupervisor,
          __MODULE__,
          :test_function,
          [:start_child_with_span]
        )
      end

      assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
      assert_receive {:span, span(parent_span_id: ^root_span_id, name: "task span", attributes: attrs)}

      assert %{a: 1, value: :start_child_with_span} == attributes(attrs)
    end

    test "start_child_with_linked_span" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.start_child_with_linked_span(
          "linked task span",
          %{attributes: %{a: 1}},
          Test.TaskSupervisor,
          fn ->
            Tracer.set_attribute(:value, :start_child_with_linked_span)

            :ok
          end
        )
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, attributes: attrs, links: links, name: "linked task span")}

      assert %{a: 1, value: :start_child_with_linked_span} == attributes(attrs)
      assert [] != links
    end

    test "start_child_with_linked_span_mfa" do
      start_supervised!({Task.Supervisor, name: Test.TaskSupervisor})

      Tracer.with_span "parent span" do
        Task.Supervisor.start_child_with_linked_span(
          "linked task span",
          %{attributes: %{a: 1}},
          Test.TaskSupervisor,
          __MODULE__,
          :test_function,
          [
            :start_child_with_linked_span
          ]
        )
      end

      assert_receive {:span, span(name: "parent span")}

      assert_receive {:span,
                      span(parent_span_id: :undefined, links: links, attributes: attrs, name: "linked task span")}

      assert %{a: 1, value: :start_child_with_linked_span} == attributes(attrs)
      assert [] != links
    end
  end

  def test_function(value) do
    Tracer.set_attributes(%{value: value})

    value
  end

  def ctx_test_function(value) do
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
