# opentelemetry_beam

Metrics collector to export statistics from the BEAM VM.

The metrics and their names are heavily borrowed from [prometheus.erl](https://github.com/deadtrickster/prometheus.erl).

After installing, setup the desired metrics in your application behaviour before your
top-level supervisor starts. Make sure the API and SDK applications are started before
your application.

```erlang
opentelemetry_beam_metrics:setup(),
...
```

Metrics that are based on microstate accounting need to be enabled explicitly with the `msacc` setting in the `opt_in` option:

```erlang
opentelemetry_beam_metrics:setup(#{opt_in => #{msacc => true}}),
...
```
