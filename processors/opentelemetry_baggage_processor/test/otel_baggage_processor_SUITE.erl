-module(otel_baggage_processor_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

all() ->
  [baggage_handling, add_prefix_to_attributes, filter_baggage_attributes].

init_per_suite(Config) ->
  ok = application:load(opentelemetry_baggage_processor),
  ok = application:load(opentelemetry),
  application:set_env(opentelemetry,
                      processors,
                      [{otel_baggage_processor, #{}},
                       {otel_batch_processor, #{scheduled_delay_ms => 1}}]),
  Config.

end_per_suite(_Config) ->
  ok = application:unload(opentelemetry),
  ok.

init_per_testcase(_, Config) ->
  {ok, _} = application:ensure_all_started(opentelemetry_baggage_processor),
  Config.

end_per_testcase(_, Config) ->
  application:stop(opentelemetry),
  Config.

baggage_handling(_Config) ->
  {ok, _} = application:ensure_all_started(opentelemetry),
  otel_batch_processor:set_exporter(otel_exporter_pid, self()),
  SpanCtx1 = ?start_span(<<"span-1">>),
  ?set_current_span(SpanCtx1),
  Ctx = otel_ctx:get_current(),
  Ctx2 = otel_baggage:set(Ctx, <<"key">>, <<"value">>),
  _Token = otel_ctx:attach(Ctx2),
  SpanCtx2 =
    ?start_span(<<"span-2">>, #{attributes => #{<<"existing-attribute">> => true}}),
  ?end_span(),
  ?set_current_span(SpanCtx2),
  ?end_span(),
  Attributes = get_span_attributes(<<"span-1">>),
  ?assertEqual(Attributes, #{}),
  Attributes2 = get_span_attributes(<<"span-2">>),
  ?assertEqual(Attributes2, #{<<"key">> => <<"value">>, <<"existing-attribute">> => true}),
  ok.

add_prefix_to_attributes(_Config) ->
  application:set_env(opentelemetry,
                      processors,
                      [{otel_baggage_processor, #{prefix => <<"app.">>}},
                       {otel_batch_processor, #{scheduled_delay_ms => 1}}]),
  {ok, _} = application:ensure_all_started(opentelemetry),
  otel_batch_processor:set_exporter(otel_exporter_pid, self()),
  Ctx = otel_ctx:get_current(),
  Ctx2 = otel_baggage:set(Ctx, <<"key">>, <<"value">>),
  Ctx3 = otel_baggage:set(Ctx2, atom_key, <<"value">>),
  _Token = otel_ctx:attach(Ctx3),
  SpanCtx1 = ?start_span(<<"span-1">>),
  ?set_current_span(SpanCtx1),
  ?end_span(),
  Attributes = get_span_attributes(<<"span-1">>),
  ?assertEqual(#{<<"app.key">> => <<"value">>, <<"app.atom_key">> => <<"value">>},
               Attributes),
  ok.

filter_baggage_attributes(_Config) ->
  application:set_env(opentelemetry,
                      processors,
                      [{otel_baggage_processor, #{filter => <<"trace_field">>}},
                       {otel_batch_processor, #{scheduled_delay_ms => 1}}]),
  {ok, _} = application:ensure_all_started(opentelemetry),
  otel_batch_processor:set_exporter(otel_exporter_pid, self()),
  Ctx = otel_ctx:get_current(),
  Ctx2 = otel_baggage:set(Ctx, <<"key">>, <<"value">>),
  Ctx3 = otel_baggage:set(Ctx2, atom_key, <<"value">>, [<<"trace_field">>]),
  _Token = otel_ctx:attach(Ctx3),
  SpanCtx1 = ?start_span(<<"span-1">>),
  ?set_current_span(SpanCtx1),
  ?end_span(),
  Attributes = get_span_attributes(<<"span-1">>),
  ?assertEqual(#{<<"atom_key">> => <<"value">>}, Attributes),
  ok.

get_span_attributes(Name) ->
  receive
    {span, #span{name = Name, attributes = Attributes}} ->
      otel_attributes:map(Attributes)
  after 100 ->
    error(timeout)
  end.