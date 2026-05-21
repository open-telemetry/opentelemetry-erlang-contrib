# OpenTelemetry Process Instrumentation

NIF code is based on https://github.com/deadtrickster/prometheus_process_collector

OpenTelemtry instrumentation with metrics of the current state of cpu, memory, file descriptor usage and native threads count as well as the process start and up times. Implements all process metrics from OpenTelemetry's semantic convertions and some more.

- FreeBSD;
- Linux - uses /proc;
- MacOS X (expiremental).

After installing, setup the desired metrics in your application behaviour before your
top-level supervisor starts. Make sure the API and SDK applications are started before
your application.

```erlang
opentelemetry_process_metrics:setup(),
...
```

Build
-----

    $ rebar3 compile

License
-----

FreeBSD-specific part uses copy-modified code from standard utils (limits and procstat) or standard API in some places.

MIT
