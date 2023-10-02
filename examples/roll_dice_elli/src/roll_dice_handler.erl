-module(roll_dice_handler).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-include("roll_dice_instruments.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_meter.hrl").

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"rolldice">>], _Req) ->
    ?update_name(<<"GET /rolldice">>),
    Roll = do_roll(),
    {ok, [], erlang:integer_to_binary(Roll)};
handle('GET', [], _Req) ->
    ?update_name(<<"GET /">>),
    [{_, TraceParent}] = otel_propagator_text_map:inject([]),
    {ok, [], index(TraceParent)};
handle('GET', File=[<<"static">>, _], _Req) ->
    PrivDir = code:priv_dir(roll_dice),
    Filename = filename:join([PrivDir | File]),
    {ok, [], {file, Filename}}.

handle_event(_Event, _Data, _Args) ->
    ok.

%%

-spec do_roll() -> integer().
do_roll() ->
    ?with_span(dice_roll, #{},
               fun(_) ->
                       Roll = rand:uniform(6),
                       ?set_attribute('roll.value', Roll),
                       ?counter_add(?ROLL_COUNTER, 1, #{'roll.value' => Roll}),
                       Roll
               end).

-spec index(unicode:unicode_binary()) -> iolist().
index(TraceContext) ->
    [<<"<html>
          <head>
            <script src=\"/static/index.js\"> </script>
            <meta name=\"traceparent\" content=\"">>, TraceContext, <<"\" />
          </head>
          <body>
            <div id=\"dice-result-div\">0</div>

            <button hx-get=\"/rolldice\" hx-target=\"#dice-result-div\" hx-swap=\"innerHTML\">
            Roll
            </button>

          </body>
        </html>">>].
