-module(opentelemetry_aws_xray_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [generate_trace_id_should_be_xray_compatible, generate_span_id_should_be_default].

generate_trace_id_should_be_xray_compatible(_Config) ->
    MaxAge = 60 * 60 * 24 * 28,
    MaxSkew = 60 * 5,

    TraceId = otel_aws_xray_id_generator:generate_trace_id(),
    EncodedTraceId = binary:encode_unsigned(TraceId),

    EpochNow = os:system_time(second),

    % The Trace id should have 128 bits (16 bytes)
    ?assertEqual(16, byte_size(EncodedTraceId)),

    % The first 4 bytes is the epoch
    Epoch = binary:decode_unsigned(binary:part(EncodedTraceId, 0, 4)),
    Delta = EpochNow - Epoch,

    ?assertEqual(false, (Delta > MaxAge) or (Delta < -MaxSkew)).

generate_span_id_should_be_default(_Config) ->
    SpanId = otel_aws_xray_id_generator:generate_span_id(),

    ?assertEqual(8, byte_size(binary:encode_unsigned(SpanId))).
