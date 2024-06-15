-module(otel_baggage_processor).

-behaviour(otel_span_processor).

-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").

-export([on_start/3, on_end/2, force_flush/1]).

-type processor_config() :: term().

-spec on_start(otel_ctx:t(), opentelemetry:span(), processor_config()) ->
                opentelemetry:span().
on_start(Ctx, Span, Config) ->
  Baggage = otel_baggage:get_all(Ctx),
  Attributes =
    maps:fold(fun(Key, {Value, Metadata}, Attributes) ->
                 NewKey = add_prefix(Key, Config),
                 case filter(Metadata, Config) of
                   false -> Attributes;
                   true -> [{NewKey, Value} | Attributes]
                 end
              end,
              [],
              Baggage),
  add_attributes(Span, Attributes).

-spec on_end(opentelemetry:span(), processor_config()) ->
              true | dropped | {error, invalid_span} | {error, no_export_buffer}.
on_end(_Span, _Config) ->
  true.

-spec force_flush(processor_config()) -> ok | {error, term()}.
force_flush(_Config) ->
  ok.

-spec add_attributes(opentelemetry:span(), opentelemetry:attributes_map()) ->
                      opentelemetry:span().
add_attributes(Span = #span{attributes = SpanAttributes}, AttributesMap) ->
  Span#span{attributes = otel_attributes:set(AttributesMap, SpanAttributes)}.

-spec filter(otel_baggage:metadata(), map()) -> boolean().
filter(Metadata, #{filter := FilterKey}) ->
  case lists:search(fun (Key) when Key == FilterKey ->
                         true;
                       (_) ->
                         false
                   end,
                   Metadata) of
    false ->
      false;
    {value, _} ->
      true
  end;
filter(_Metadata, _Config) ->
  true.

-spec add_prefix(opentelemetry:attribute_key(), map()) -> opentelemetry:attribute_key().
add_prefix(Key, #{prefix := Prefix}) when is_binary(Key), is_binary(Prefix) ->
  <<Prefix/binary, Key/binary>>;
add_prefix(Key, #{prefix := Prefix}) when is_atom(Key), is_binary(Prefix) ->
  Key2 = atom_to_binary(Key),
  <<Prefix/binary, Key2/binary>>;
add_prefix(Key, _Config) ->
  Key.
