# Phoenix + Ecto OpenTelemetry Example

This is a example repository that demo how to setup OpenTelemetry for Phoenix application
with [`opentelemetry_phoenix`][0] and [`opentelemetry_ecto`][1].

Here, we are using [`opentelemetry_exporter`][2] to export the traces to [
OpenTelemetry Collector][3]. The collector in turn export the traces to [Zipkin][4] and [
Jaeger][5] respectively.

Additionally, we also include the OpenTelemetry Collector and
`opentelemetry_exporter` configuration to
export the traces to external services like [Honeycomb](https://www.honeycomb.io/) and
[Lightstep](https://lightstep.com/).

## Getting Stated

By default, we only configure our OpenTelemetry collector to export traces to
the local Zipkin and Jaeger.

Assuming you already have Docker and Docker Compose installed:

1. Run `docker-compose up` to start the Phoenix application, PostgreSQL,
   OpenTelemetry Collector, Zipkin and Jaeger.
2. Run migration on another terminal with:
   ```
   docker exec -it basic_phoenix_ecto_phoenix_1 /bin/bash ./bin/demo eval 'Demo.Release.migrate()'
   ```
3. Browse to http://localhost:4000. Additionally, you can:

- Visit http://localhost:4000/posts to see how it works for Phoenix HTML
- Visit http://localhost:4000/users to see how it works for Phoenix LiveView

4. Visit Zipkin at http://localhost:9411 and hit `Run Query` to look the the sample trace.
5. Visit Jaeger UI at http://localhost:16686, select `demo` under Service and click `Find Trace` to
   look at the sample trace.
6. Run `docker-compose down` to destroy the created resources.

[0]: https://hex.pm/packages/opentelemetry_phoenix
[1]: https://hex.pm/packages/opentelemetry_ecto
[2]: https://hex.pm/packages/opentelemetry_exporter
[3]: https://github.com/open-telemetry/opentelemetry-collector/
[4]: https://zipkin.io/
[5]: https://www.jaegertracing.io/
