%%%-------------------------------------------------------------------
%% @doc Client module for grpc service routeguide.RouteGuide.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2022-03-16T12:32:26+00:00 and should not be modified manually

-module(routeguide_route_guide_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'routeguide.RouteGuide').
-define(PROTO_MODULE, 'route_guide_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec get_feature(route_guide_pb:point()) ->
    {ok, route_guide_pb:feature(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_feature(Input) ->
    get_feature(ctx:new(), Input, #{}).

-spec get_feature(ctx:t() | route_guide_pb:point(), route_guide_pb:point() | grpcbox_client:options()) ->
    {ok, route_guide_pb:feature(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_feature(Ctx, Input) when ?is_ctx(Ctx) ->
    get_feature(Ctx, Input, #{});
get_feature(Input, Options) ->
    get_feature(ctx:new(), Input, Options).

-spec get_feature(ctx:t(), route_guide_pb:point(), grpcbox_client:options()) ->
    {ok, route_guide_pb:feature(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
get_feature(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/routeguide.RouteGuide/GetFeature">>, Input, ?DEF(point, feature, <<"routeguide.Point">>), Options).

%% @doc 
-spec list_features(route_guide_pb:rectangle()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
list_features(Input) ->
    list_features(ctx:new(), Input, #{}).

-spec list_features(ctx:t() | route_guide_pb:rectangle(), route_guide_pb:rectangle() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
list_features(Ctx, Input) when ?is_ctx(Ctx) ->
    list_features(Ctx, Input, #{});
list_features(Input, Options) ->
    list_features(ctx:new(), Input, Options).

-spec list_features(ctx:t(), route_guide_pb:rectangle(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
list_features(Ctx, Input, Options) ->
    grpcbox_client:stream(Ctx, <<"/routeguide.RouteGuide/ListFeatures">>, Input, ?DEF(rectangle, feature, <<"routeguide.Rectangle">>), Options).

%% @doc 
-spec record_route() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
record_route() ->
    record_route(ctx:new(), #{}).

-spec record_route(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
record_route(Ctx) when ?is_ctx(Ctx) ->
    record_route(Ctx, #{});
record_route(Options) ->
    record_route(ctx:new(), Options).

-spec record_route(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
record_route(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/routeguide.RouteGuide/RecordRoute">>, ?DEF(point, route_summary, <<"routeguide.Point">>), Options).

%% @doc 
-spec route_chat() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
route_chat() ->
    route_chat(ctx:new(), #{}).

-spec route_chat(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
route_chat(Ctx) when ?is_ctx(Ctx) ->
    route_chat(Ctx, #{});
route_chat(Options) ->
    route_chat(ctx:new(), Options).

-spec route_chat(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
route_chat(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/routeguide.RouteGuide/RouteChat">>, ?DEF(route_note, route_note, <<"routeguide.RouteNote">>), Options).

%% @doc Unary RPC
-spec generate_error(route_guide_pb:empty()) ->
    {ok, route_guide_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
generate_error(Input) ->
    generate_error(ctx:new(), Input, #{}).

-spec generate_error(ctx:t() | route_guide_pb:empty(), route_guide_pb:empty() | grpcbox_client:options()) ->
    {ok, route_guide_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
generate_error(Ctx, Input) when ?is_ctx(Ctx) ->
    generate_error(Ctx, Input, #{});
generate_error(Input, Options) ->
    generate_error(ctx:new(), Input, Options).

-spec generate_error(ctx:t(), route_guide_pb:empty(), grpcbox_client:options()) ->
    {ok, route_guide_pb:empty(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
generate_error(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/routeguide.RouteGuide/GenerateError">>, Input, ?DEF(empty, empty, <<"routeguide.Empty">>), Options).

%% @doc 
-spec streaming_generate_error(route_guide_pb:empty()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
streaming_generate_error(Input) ->
    streaming_generate_error(ctx:new(), Input, #{}).

-spec streaming_generate_error(ctx:t() | route_guide_pb:empty(), route_guide_pb:empty() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
streaming_generate_error(Ctx, Input) when ?is_ctx(Ctx) ->
    streaming_generate_error(Ctx, Input, #{});
streaming_generate_error(Input, Options) ->
    streaming_generate_error(ctx:new(), Input, Options).

-spec streaming_generate_error(ctx:t(), route_guide_pb:empty(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
streaming_generate_error(Ctx, Input, Options) ->
    grpcbox_client:stream(Ctx, <<"/routeguide.RouteGuide/StreamingGenerateError">>, Input, ?DEF(empty, empty, <<"routeguide.Empty">>), Options).

