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

# How It Works

## OpenTelemetry Setup

In `rebar.config` the dependency on `opentelemetry` and
`opentelemetry_experimental` found in both `deps` as well as `shell` and `relx`
ensures the OpenTelemetry SDK and Experimental SDK are part of the build and run
of the project. The order of the lists of apps given to `shell` and `relx` are
important here:

```erlang
{shell, [{apps, [opentelemetry_exporter, 
                 opentelemetry_experimental,                  
                 opentelemetry, 
                 recon,
                 roll_dice]},
         {config, "config/sys.config"}]}.

{relx, [{release, {roll_dice, "0.1.0"},
         [opentelemetry_exporter,
          opentelemetry_experimental,
          opentelemetry,
          recon,
          roll_dice,
          sasl]}
]}.
```

These configurations tell Erlang to boot the exporter Application before the SDK
both when running with `rebar3 shell` and when building an OTP Release and
booting it. The exporter must start before the SDK to guarantee the modules are
loaded before the SDK will call the exporter modules to initialize any OTLP
exporter used.

The order also guarantees that on shutdown our application, `roll_dice`, is
stopped before OpenTelemetry and that the exporter is shutdown only after the
SDK has been.

The `applications` list in `roll_dice.app.src` should only need to specify the
OpenTelemetry API since that is all it uses, which is expected for the end user
application code.

## Traces

Spans are created both by the `opentelemetry_elli` instrumentation library and
by the handler module that handles each incoming HTTP request.
`opentelemetry_elli` is integrated into the application as a middleware:

```erlang
ElliOpts = [{callback, elli_middleware},
            {callback_args, [{mods, [{otel_elli_middleware, []},
                                     {roll_dice_handler, []}]}]},
            {port, Port}],

ChildSpecs = [#{id => roll_dice_http,
                start => {elli, start_link, [ElliOpts]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [roll_dice_handler]}],
```

These options tell Elli to use `elli_middleware` for each request, this module
will then call `otel_elli_middleware` followed by `roll_dice_handler`.

For each request the `elli_middleware` handler will first attempt to extract
trace context from the request headers. It will then start a new span, with
the span extracted from the trace context as the parent if it is available.

Because Elli has no router these spans need to have their name updated within
`roll_dice_handler` to be more useful than simply being the HTTP method. Elli by
default names spans only with the HTTP method name because the URL path is a
high cardinality value submitted by the end user.

```erlang
handle('GET', [<<"rolldice">>], _Req) ->
    ?update_name(<<"GET /rolldice">>),
...
handle('GET', [], _Req) ->
    ?update_name(<<"GET /">>),
```

This handler code uses the `?update_name` macro to update the name of the
current active span in the process dictionary context.

## Metrics
