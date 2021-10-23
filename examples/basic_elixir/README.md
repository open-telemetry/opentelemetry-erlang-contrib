# Basic Elixir Example

This is a sample repository that demo how to setup a basic Elixir application
with `opentelemetry-api` and `opentelemetry_exporter`. Here, we are using
`opentelemetry_exporter` to export the traces to [OpenTelemetry Collector][0].
The collector in turn export the traces to [Zipkin][1] and [Jaeger][2]
respectively.

## Getting Stated

Assuming you already have Docker and Docker Compose installed:

1. Run `docker-compose up` to start the application, OpenTelemetry Collector,
   Zipkin and Jaeger.
2. Visit Zipkin at http://localhost:9411 and hit `Run Query` to look the the sample trace.
3. Visit Jaeger UI at http://localhost:16686 and click `Find Trace` to look at the sample
   trace.

[0]: https://github.com/open-telemetry/opentelemetry-collector/
[1]: https://zipkin.io/
[2]: https://www.jaegertracing.io/
