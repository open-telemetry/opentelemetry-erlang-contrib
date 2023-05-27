roll_dice
=====

Run the project:

```
$ OTEL_EXPORTER_OTLP_PROTOCOL=grpc OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317 rebar3 shell
```

Roll a die:

```
$ curl localhost:3000/rolldice
4
```

Or go to `localhost:3000` in a browser to roll.
