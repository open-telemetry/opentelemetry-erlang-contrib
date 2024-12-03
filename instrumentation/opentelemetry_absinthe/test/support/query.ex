defmodule OpentelemetryAbsintheTest.Support.Query do
  @moduledoc false

  import ExUnit.Assertions
  alias OpentelemetryAbsintheTest.Support.GraphQL.Schema
  require Record
  require Logger

  @fields Record.extract(:span, from: "deps/opentelemetry/include/otel_span.hrl")
  Record.defrecordp(:span, @fields)

  def query_for_span(query, opts \\ []) do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    {:ok, data} = Absinthe.run(query, Schema, opts)
    Logger.debug("Absinthe query returned: #{inspect(data)}")

    assert_receive {:span, span}, 5000
    Logger.debug("Recieved span: #{inspect(span)}")
    span
  end

  def query_for_attrs(query, opts \\ []) do
    span(attributes: {_, _, _, _, attributes}) = query_for_span(query, opts)
    attributes
  end

  def query_for_span_name(query, opts \\ []) do
    span(name: name) = query_for_span(query, opts)
    name
  end

  def query_for_span_kind(query, opts \\ []) do
    span(kind: kind) = query_for_span(query, opts)
    kind
  end
end
