# opentelemetry_diameter

Metrics collector to export statistics from the diameter application.

The metrics and their names are heavily borrowed from [prometheus.erl](https://github.com/deadtrickster/prometheus.erl).

After installing, setup the desired metrics in your application behaviour before your
top-level supervisor starts. Make sure the API and SDK applications are started before
your application.

```erlang
opentelemetry_diameter_metrics:setup(),
...
```
