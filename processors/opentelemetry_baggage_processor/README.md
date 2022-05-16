# opentelemetry_baggage_processor

A Span Processor that takes attributes from the Baggage and insert into the Span.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_baggage_processor` to your list of dependencies:

```erlang
{deps, [
  {opentelemetry_baggage_processor, "~> 0.1"}
]}.
```

```elixir
def deps do
  [
    {:opentelemetry_baggage_processor, "~> 0.1"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_baggage_processor](https://hexdocs.pm/opentelemetry_baggage_processor).

## Usage

<!-- MDOC -->

`opentelemetry_baggage_processor` provides a [Span Processor](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md#span-processor)
that takes attributes from the [Baggage](https://hexdocs.pm/opentelemetry_api/otel_baggage.html)
and insert into the Span, once it starts.

A Span Processor is not an application, to use it you must update your
configuration:

```elixir
# config/config.exs

config :opentelemetry, processors,
  otel_baggage_processor: %{}},
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{}}
  }
```

The processor configuration is order-dependent, so `otel_baggage_processor` 
configuration must come before the processor used for exporting – in this case, 
`otel_batch_processor`.

Now every new span should have what's inside your baggage as attribute.

### Options

* `:prefix` - adds a prefix for all baggage attributes.
* `:filter` - only add attributes if the baggage metadata has the configured key.
The key must be a binary.

### Limitations

Baggage will follow the Context. So any limitation to Context Propagation applies
to Baggage Propagation, and thus to what attributes are going to be added to your
Span.

We can only apply attributes on Span's start, since that's when we can modify
them. There's a [BeforeEnd callback proposal](https://github.com/open-telemetry/opentelemetry-specification/issues/1089)
which would allow us to add the Baggage's attribute on Span's end too, but that
remains as something to be revisited in the future.

<!-- MDOC -->
