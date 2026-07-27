-module(otel_aws_xray_id_generator).

-behavior(otel_id_generator).

-export([
    generate_trace_id/0,
    generate_span_id/0
]).

%%    Generates a trace_id compatible with AWS X-Ray
%%
%%    See More: https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids
%%
%%    The first 32 bits is the timestamp, the remaining 96 bits are random
-spec generate_trace_id() -> opentelemetry:trace_id().
generate_trace_id() ->
    Random = rand:uniform(2 bsl (95 - 1)),
    TimeStamp = os:system_time(second),

    HiBytes = binary:encode_unsigned(TimeStamp),
    LoBytes = binary:encode_unsigned(Random),

    TraceIdBytes = <<HiBytes/binary, LoBytes/binary>>,

    binary:decode_unsigned(TraceIdBytes).

-spec generate_span_id() -> opentelemetry:span_id().
generate_span_id() ->
    rand:uniform(2 bsl 63 - 1).
