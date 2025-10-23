roll_dice
=====

Start the OpenTelemetry collector and Jaeger with Docker Compose:

```
$ docker compose up
```

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

View traces in Jaeger at [http://localhost:16686](http://localhost:16686).d

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

In `sys.config` the configuration for the OpenTelemetry SDK can be found:

```erlang
 {opentelemetry,
  [{span_processor, batch},
   {traces_exporter, otlp}
  ]},
```

This has the SDK setup to use the batch processor which batches up spans
together before passing them on to the exporter, and to use the OTLP exporter.
Defaults for the exporter are used since we just have it running in Docker on
the default port and localhost.

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

The core logic of the application is `do_roll`, shown here without any calls to
metric related functions/macros:

```
-spec do_roll() -> integer().
do_roll() ->
    ?with_span(dice_roll, #{},
               fun(_) ->
                       Roll = rand:uniform(6),
                       ?set_attribute('roll.value', Roll),
                       Roll
               end).
```

The `?with_span` macro will start a new span named `dice_roll` with no
attributes (`#{}`) and set it to be the active span within the process
dictionary context while the anonymous function is run. When that function
completes the span is ended. Within the body of that function a random number is
generated and used as the attribute value for a span attribute `roll.value`.
This attribute is added through the macro `?set_attribute` that will add an
attribute to the currently active span in the process dictionary.

## Metrics

For metrics the Experimental SDK is configured in `sys.config`:

```erlang
{opentelemetry_experimental,
  [{readers, [#{module => otel_metric_reader,
                config => #{export_interval_ms => 1000,
                            exporter => {otel_metric_exporter_console, #{}}}}]}]},
```

This configures the SDK to have a single metric reader, using module
`otel_metric_reader`, that is configured to export metrics every second with an
exporter that writes to stdout. This results in output to the console showing
the metric name along with its attributes followed by the aggregate value, like:

```
roll_counter{roll.value=5} 1
```

Instruments have names which allow you to reference them from anywhere in your
code you have that name. To help with this there is `roll_dice_instruments.hrl`:

```erlang
-define(ROLL_COUNTER, roll_counter).
```

To initialize the instruments used a call to `create_instruments/0` is added in
`start/2` of `roll_dice_app`:

```erlang
start(_StartType, _StartArgs) ->
    create_instruments(),
    roll_dice_sup:start_link().

create_instruments() ->
    ?create_counter(?ROLL_COUNTER, #{description => <<"The number of rolls by roll value.">>,
                                     unit => '1'}).
```

This means that when the `roll_dice` Application boots it will first create an
instrument using the `?create_counter` macro and name it `?ROLL_COUNTER`.

In the handler, `roll_dice_handler`, the header `roll_dice_instruments.hrl` is
included so the counter can be incremented with the result of the roll as an
attribute on the measurement:

```erlang
-spec do_roll() -> integer().
do_roll() ->
    ?with_span(dice_roll, #{},
               fun(_) ->
                       Roll = rand:uniform(6),
                       ?set_attribute('roll.value', Roll),
                       ?counter_add(?ROLL_COUNTER, 1, #{'roll.value' => Roll}),
                       Roll
               end).
```
