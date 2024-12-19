defmodule OpentelemetrySqlcommenter do
  @moduledoc """
  Adds OpenTelemetry trace context to SQL queries as comments.

  OpentelemetrySqlcommenter is an Elixir library that automatically adds
  OpenTelemetry trace context to your SQL queries as comments. This enables
  better observability by linking database operations with distributed
  traces.

  > **Note:** Currently only compatible with PostgreSQL databases.

  ## Features

  - Automatically injects OpenTelemetry trace context into SQL queries
  - Compatible with Ecto repositories using PostgreSQL
  - Follows W3C Trace Context specification
  - Minimal performance overhead
  - Easy integration with existing projects

  ## Installation

  Add `opentelemetry_sqlcommenter` to your list of dependencies in `mix.exs`:

  ```elixir
  def deps do
    [
      {:opentelemetry_sqlcommenter, "~> 0.1.1"},
    ]
  end
  ```

  ## Usage

  1. In your Ecto repository, delegate the `prepare_query` function to OpentelemetrySqlcommenter:

  ```elixir
  defmodule YourApp.Repo do
    use Ecto.Repo,
      otp_app: :your_app,
      adapter: Ecto.Adapters.Postgres

    defdelegate prepare_query(operation, query, opts), to: OpentelemetrySqlcommenter
  end
  ```

  2. That's it! Your SQL queries will now include trace context as comments:

  ```
  SELECT u1."id", u1."email" FROM "user_token"
  AS u0 INNER JOIN "user" AS u1 ON u1."id" = u0."user_id"
  WHERE ((u0."token" = $1) AND (u0."context" = $2)) AND
  (u0."inserted_at" > $3::timestamp + (-(60)::numeric * interval '1 day'))
  /*traceparent='00-6b6bcfabaea5ef9890481e9b0c18efac-4deda0f3c249b6c7-01'*/
  ```

  ## Configuration

  No additional configuration is needed if you already have OpenTelemetry set up in your application.

  ## How It Works

  The library:
  1. Intercepts queries before they're sent to the database
  2. Checks for active OpenTelemetry trace context
  3. If present, adds the trace context as a SQL comment
  4. Preserves all existing query options

  ## Important Disclaimer

  ⚠️ **Impact on Query Performance**

  This library disables prepared statements and query caching by setting
  `prepare: :unnamed` for all queries. This is necessary because:

  1. SQL comments make each query unique, even if the underlying SQL is identical
  2. Prepared statements and caching rely on query text matching exactly
  3. Adding trace context makes each query instance unique,
  defeating the purpose of preparation and caching

  ### Performance Implications

  - **Prepared Statements**: Normally, Postgres can prepare and cache
  query execution plans. With this library, each query is treated as new,
  requiring a new execution plan.

  ### When to Use

  Use this library when:
  - Trace context in SQL logs is more important than maximum query performance
  - You're debugging or need detailed observability
  - Your application's performance isn't critically dependent on prepared statements

  Consider disabling it in performance-critical environments or creating a configuration option to toggle it based on your needs.

  ### Alternative Approaches

  If you need both tracing and prepared statements, consider:
  1. Using separate logging mechanisms for trace context
  2. Implementing selective tracing for specific queries only
  3. Using database-specific audit logging features

  ## Related Links

  - [SQL Commenter with Postgrex](https://dev.to/dkuku/sql-commenter-with-postgrex-2bfd)
  - [OpenTelemetry Documentation](https://opentelemetry.io/docs/)
  - [W3C Trace Context Specification](https://www.w3.org/TR/trace-context/)

  Adds OpenTelemetry trace context to SQL queries as comments.

  This module is designed to be used with Ecto repositories to automatically inject
  OpenTelemetry trace information into SQL queries. It adds a SQL comment containing
  the W3C trace context (traceparent) to each query, enabling correlation between
  database operations and distributed traces.

  ## Usage

  In your Ecto repository, add:

      defmodule YourApp.Repo do
        use Ecto.Repo,
          otp_app: :your_app,
          adapter: Ecto.Adapters.Postgres

        defdelegate prepare_query(operation, query, opts), to: OpentelemetrySqlcommenter
      end

  ## Format

  The module adds SQL comments in the following format:

  ```
  YOUR QUERY/*traceparent='00-trace_id-span_id-flags'*/
  ```
  """

  @doc """
  Prepares a query by adding OpenTelemetry trace context as a SQL comment for any active span.

  Similar to `prepare_query_sampled/3` but adds trace context for all traces regardless of sampling status.
  Designed to be used with `defdelegate` in your Ecto repository when you want to track all queries.

  ## Setup

      defmodule MyApp.Repo do
        use Ecto.Repo,
          otp_app: :my_app,
          adapter: Ecto.Adapters.Postgres

        defdelegate prepare_query(operation, query, opts), to: OpentelemetrySqlcommenter
      end

  ## Parameters

    * `_operation` - The type of operation (unused, maintained for Ecto compatibility)
    * `query` - The SQL query string
    * `opts` - Query options keyword list

  ## Returns

    * `{modified_query, modified_opts}` - Tuple containing the query and modified options with trace context
    * `{query, opts}` - Original query and options if no active sampled trace is present

  Note: This function disables prepared statements by setting `prepare: :unnamed` when adding trace context.
  """
  def prepare_query(_operation, query, opts) do
    with span_ctx when elem(span_ctx, 0) == :span_ctx <- OpenTelemetry.Tracer.current_span_ctx(),
         traceparent when is_binary(traceparent) <- build_traceparent(span_ctx) do
      comment = "traceparent='#{traceparent}'"
      {query, [comment: comment, prepare: :unnamed] ++ opts}
    else
      _ -> {query, opts}
    end
  end

  @doc """
  Prepares a query by adding OpenTelemetry trace context as a SQL comment, but only for sampled traces.

  This function is designed to be used with `defdelegate` in your Ecto repository to automatically
  add trace context to all database queries when sampling is enabled (trace_flags = 1).

  ## Setup

      defmodule MyApp.Repo do
        use Ecto.Repo,
          otp_app: :my_app,
          adapter: Ecto.Adapters.Postgres

        defdelegate prepare_query(operation, query, opts), to: OpentelemetrySqlcommenter, as: :prepare_query_sampled
      end

  ## Parameters

    * `_operation` - The type of operation (unused, maintained for Ecto compatibility)
    * `query` - The SQL query string
    * `opts` - Query options keyword list

  ## Returns

    * `{modified_query, modified_opts}` - Tuple containing the query and modified options with trace context
    * `{query, opts}` - Original query and options if no active trace is present

  Note: This function disables prepared statements by setting `prepare: :unnamed` when adding trace context.
  """
  def prepare_query_sampled(_operation, query, opts) do
    with {:span_ctx, _, _, _trace_flags = 1, _, _, _, _, _} = span_ctx <-
           OpenTelemetry.Tracer.current_span_ctx(),
         traceparent when is_binary(traceparent) <- build_traceparent(span_ctx) do
      comment = "traceparent='#{traceparent}'"
      {query, [comment: comment, prepare: :unnamed] ++ opts}
    else
      _ -> {query, opts}
    end
  end

  defp build_traceparent(
         {:span_ctx, trace_id, span_id, trace_flags, _tracestate, _is_valid, _is_remote,
          _has_remote_parent, _impl}
       ) do
    version = "00"
    trace_id = encode_trace_id(trace_id)
    span_id = encode_span_id(span_id)
    trace_flags = encode_flags(trace_flags)

    "#{version}-#{trace_id}-#{span_id}-#{trace_flags}"
  end

  defp build_traceparent(_), do: nil

  defp encode_trace_id(trace_id) when is_integer(trace_id) do
    trace_id
    |> :binary.encode_unsigned()
    |> Base.encode16(case: :lower)
    |> String.pad_leading(32, "0")
  end

  defp encode_span_id(span_id) when is_integer(span_id) do
    span_id
    |> :binary.encode_unsigned()
    |> Base.encode16(case: :lower)
    |> String.pad_leading(16, "0")
  end

  defp encode_flags(1), do: "01"
  defp encode_flags(0), do: "00"

  defp encode_flags(trace_flags) when is_integer(trace_flags) do
    Integer.to_string(trace_flags, 16) |> String.pad_leading(2, "0")
  end
end
