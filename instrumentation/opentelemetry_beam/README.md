# opentelemetry_beam

Metrics collector to export statistics from the BEAM VM.

The metrics and their names are heavily borrowed from [prometheus.erl](https://github.com/deadtrickster/prometheus.erl).

After installing, setup the desired metrics in your application behaviour before your
top-level supervisor starts. Make sure the API and SDK applications are started before
your application.

```erlang
opentelemetry_vm_memory:setup(),
opentelemetry_vm_msacc:setup(),
opentelemetry_vm_statistics:setup(),
opentelemetry_vm_system_info:setup(),
...
```
