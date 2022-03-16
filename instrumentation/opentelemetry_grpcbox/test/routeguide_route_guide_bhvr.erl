%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service routeguide.RouteGuide.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2022-03-16T12:32:26+00:00 and should not be modified manually

-module(routeguide_route_guide_bhvr).

%% @doc Unary RPC
-callback get_feature(ctx:ctx(), route_guide_pb:point()) ->
    {ok, route_guide_pb:feature(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc 
-callback list_features(route_guide_pb:rectangle(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

%% @doc 
-callback record_route(reference(), grpcbox_stream:t()) ->
    {ok, route_guide_pb:route_summary(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc 
-callback route_chat(reference(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback generate_error(ctx:ctx(), route_guide_pb:empty()) ->
    {ok, route_guide_pb:empty(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc 
-callback streaming_generate_error(route_guide_pb:empty(), grpcbox_stream:t()) ->
    ok | grpcbox_stream:grpc_error_response().

