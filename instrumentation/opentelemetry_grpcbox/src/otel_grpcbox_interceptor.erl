%%%------------------------------------------------------------------------
%% Copyright 2022, Tristan Sloughter
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc grpcbox interceptor for tracing grpc client and server requests.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_grpcbox_interceptor).

-export([%% server side
         unary/4,
         stream/4,

         %% unary client interceptor
         unary_client/7,

         %% client streaminig interceptors
         new_stream/6,
         send_msg/3,
         recv_msg/3]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_semantic_conventions/include/trace.hrl").

-define(RPC_SYSTEM_GRPC, 'grpc').

unary_client(Ctx, _Channel, Handler, FullMethod, Input, _Def, _Options) ->
    Metadata = otel_propagator_text_map:inject(opentelemetry:get_text_map_injector(),
                                               #{},
                                               fun set_metadata/3),
    Ctx1 = grpcbox_metadata:append_to_outgoing_ctx(Ctx, Metadata),
    ?with_span(FullMethod, #{kind => ?SPAN_KIND_CLIENT,
                             attributes => #{?RPC_SYSTEM => ?RPC_SYSTEM_GRPC}}, fun(_) ->
                                        Handler(Ctx1, Input)
                                end).

new_stream(Ctx, Channel, Path, Def, Streamer, Options) ->
    {ok, S} = Streamer(Ctx, Channel, Path, Def, Options),
    {ok, #{client_stream => S}}.

send_msg(#{client_stream := ClientStream}, Streamer, Input) ->
    Streamer(ClientStream, Input).

recv_msg(#{client_stream := ClientStream}, Streamer, Input) ->
    Streamer(ClientStream, Input).

unary(Ctx, Message, _ServerInfo=#{full_method := FullMethod}, Handler) ->
    otel_ctx_from_ctx(Ctx),
    ?with_span(FullMethod, #{kind => ?SPAN_KIND_SERVER,
                             attributes => #{?RPC_SYSTEM => ?RPC_SYSTEM_GRPC}}, fun(_) ->
                                        Handler(Ctx, Message)
                                end).

stream(Ref, Stream, _ServerInfo=#{full_method := FullMethod}, Handler) ->
    Ctx = grpcbox_stream:ctx(Stream),
    otel_ctx_from_ctx(Ctx),
    ?with_span(FullMethod, #{kind => ?SPAN_KIND_SERVER,
                             attributes => #{?RPC_SYSTEM => ?RPC_SYSTEM_GRPC}}, fun(_) ->
                                        Handler(Ref, Stream)
                                end).

%%

otel_ctx_from_ctx(Ctx) ->
    Metadata = grpcbox_metadata:from_incoming_ctx(Ctx),
    otel_propagator_text_map:extract(opentelemetry:get_text_map_extractor(),
                                     Metadata,
                                     fun maps:keys/1,
                                     fun get_metadata/2).

get_metadata(Key, Metadata) ->
    maps:get(Key, Metadata, undefined).

set_metadata(Key, Value, Metadata) ->
    Metadata#{Key => Value}.
