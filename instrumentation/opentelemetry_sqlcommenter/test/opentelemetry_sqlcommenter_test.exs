defmodule OpentelemetrySqlcommenterTest do
  use ExUnit.Case
  require OpenTelemetry.Tracer

  describe "prepare_query/3" do
    test "adds traceparent when there's an active span" do
      OpenTelemetry.Tracer.with_span "test_operation" do
        query = "SELECT * FROM users"
        opts = []

        {_modified_query, modified_opts} =
          OpentelemetrySqlcommenter.prepare_query(:all, query, opts)

        assert Keyword.has_key?(modified_opts, :comment)
        comment = Keyword.get(modified_opts, :comment)
        assert comment =~ ~r/traceparent='00-[a-f0-9]{32}-[a-f0-9]{16}-[0-9]{2}'/
        assert Keyword.get(modified_opts, :prepare) == :unnamed
      end
    end

    test "returns query unchanged when no active span" do
      query = "SELECT * FROM users"
      opts = []

      {_modified_query, modified_opts} =
        OpentelemetrySqlcommenter.prepare_query(:all, query, opts)

      assert modified_opts == opts
    end

    test "preserves existing options" do
      OpenTelemetry.Tracer.with_span "test_operation" do
        query = "SELECT * FROM users"
        opts = [timeout: 5000]

        {_modified_query, modified_opts} =
          OpentelemetrySqlcommenter.prepare_query(:all, query, opts)

        assert Keyword.get(modified_opts, :timeout) == 5000
        assert Keyword.has_key?(modified_opts, :comment)
        assert Keyword.get(modified_opts, :prepare) == :unnamed
      end
    end

    test "generated traceparent has correct format" do
      OpenTelemetry.Tracer.with_span "test_operation" do
        query = "SELECT * FROM users"

        {_modified_query, modified_opts} =
          OpentelemetrySqlcommenter.prepare_query(:all, query, [])

        comment = Keyword.get(modified_opts, :comment)
        assert comment =~ ~r/^traceparent='00-[a-f0-9]{32}-[a-f0-9]{16}-[0-9]{2}'$/

        # Extract parts of traceparent
        ~r/traceparent='00-(?<trace_id>[a-f0-9]{32})-(?<span_id>[a-f0-9]{16})-(?<flags>[0-9]{2})'/
        |> Regex.named_captures(comment)
        |> case do
          %{"trace_id" => trace_id, "span_id" => span_id, "flags" => flags} ->
            assert String.length(trace_id) == 32
            assert String.length(span_id) == 16
            assert flags in ["00", "01"]

          nil ->
            flunk("Traceparent format is incorrect")
        end
      end
    end
  end

  describe "prepare_query_sampled/3" do
    test "adds traceparent when there's an active sampled span" do
      OpenTelemetry.Tracer.with_span "test_operation" do
        query = "SELECT * FROM users"
        opts = []

        {_modified_query, modified_opts} =
          OpentelemetrySqlcommenter.prepare_query_sampled(:all, query, opts)

        assert Keyword.has_key?(modified_opts, :comment)
        comment = Keyword.get(modified_opts, :comment)
        assert comment =~ ~r/traceparent='00-[a-f0-9]{32}-[a-f0-9]{16}-[0-9]{2}'/
        assert Keyword.get(modified_opts, :prepare) == :unnamed
      end
    end

    test "returns query unchanged when no active span" do
      query = "SELECT * FROM users"
      opts = []

      {_modified_query, modified_opts} =
        OpentelemetrySqlcommenter.prepare_query_sampled(:all, query, opts)

      assert modified_opts == opts
    end

    test "preserves existing options with sampled span" do
      OpenTelemetry.Tracer.with_span "test_operation" do
        query = "SELECT * FROM users"
        opts = [timeout: 5000]

        {_modified_query, modified_opts} =
          OpentelemetrySqlcommenter.prepare_query_sampled(:all, query, opts)

        assert Keyword.get(modified_opts, :timeout) == 5000
        assert Keyword.has_key?(modified_opts, :comment)
        assert Keyword.get(modified_opts, :prepare) == :unnamed
      end
    end
  end
end
