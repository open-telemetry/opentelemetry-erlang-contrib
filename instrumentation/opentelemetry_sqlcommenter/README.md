# OpentelemetrySqlcommenter

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
2024-12-16 18:25:32.115
GMT,"postgres","ai_connector_dev",104871,"127.0.0.1:58162",67607098.199a7,1,
"SELECT",2024-12-16 18:25:28 GMT,9/78,0,LOG,00000, "execute
<unnamed>: SELECT u1.""id"", u1.""email"", FROM ""user_token""
AS u0 INNER JOIN ""user"" AS u1 ON u1.""id"" = u0.""user_id""
WHERE ((u0.""token"" = $1) AND (u0.""context"" = $2)) AND
(u0.""inserted_at"" > $3::timestamp + (-(60)::numeric * interval '1
day'))/*traceparent='00-6b6bcfabaea5ef9890481e9b0c18efac-4deda0f3c249b6c7-01'*/",
"parameters: $1 =
'\x020aec1aef94f66f64dd5213ed23e4b244cf40f4dae502ebae1d47857982b99b',
$2 = 'session', $3 = '2024-12-16 18:25:32.090091'" ,,,,,,,,"","client
backend",,0
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

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Related Links

- [SQL Commenter with Postgrex](https://dev.to/dkuku/sql-commenter-with-postgrex-2bfd)
- [OpenTelemetry Documentation](https://opentelemetry.io/docs/)
- [W3C Trace Context Specification](https://www.w3.org/TR/trace-context/)

## Documentation

Full documentation can be found at [HexDocs](https://hexdocs.pm/opentelemetry_sqlcommenter).
