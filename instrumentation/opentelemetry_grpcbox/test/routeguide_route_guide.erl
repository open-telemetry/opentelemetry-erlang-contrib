-module(routeguide_route_guide).

-include_lib("grpcbox/include/grpcbox.hrl").

-export([get_feature/2,
         list_features/2,
         record_route/2,
         route_chat/2,
         generate_error/2,
         streaming_generate_error/2]).

-type route_summary() ::
    #{point_count => integer(),
      feature_count => integer(),
      distance => integer(),
      elapsed_time => integer()}.

-type point() ::
    #{latitude => integer(),
      longitude => integer()}.

-type rectangle() ::
    #{lo => point(),
      hi => point()}.

-type route_note() ::
    #{location => point(),
      message => string()}.

-type feature() ::
    #{name => string(),
      location => point()}.

-spec get_feature(Ctx :: ctx:ctx(), Message :: point()) -> {ok, feature(), ctx:ctx()}.
get_feature(Ctx, Message) ->
    Feature = #{name => find_point(Message, data()),
                location => Message},
    {ok, Feature, Ctx}.

-spec list_features(Message::rectangle(), GrpcStream :: grpcbox_stream:t()) -> ok.
list_features(_Message, GrpcStream) ->
    grpcbox_stream:add_headers([{<<"info">>, <<"this is a test-implementation">>}], GrpcStream),
    grpcbox_stream:send(#{name => <<"Tour Eiffel">>,
                                        location => #{latitude => 3,
                                                      longitude => 5}}, GrpcStream),
    grpcbox_stream:send(#{name => <<"Louvre">>,
                          location => #{latitude => 4,
                                        longitude => 5}}, GrpcStream),

    grpcbox_stream:add_trailers([{<<"nr_of_points_sent">>, <<"2">>}], GrpcStream),
    ok.

-spec record_route(reference(), GrpcStream :: grpcbox_stream:t()) -> {ok, route_summary(), grpcbox_stream:t()}.
record_route(Ref, GrpcStream) ->
    record_route(Ref, #{t_start => erlang:system_time(1),
                        acc => []}, GrpcStream).

record_route(Ref, Data=#{t_start := T0, acc := Points}, GrpcStream) ->
    receive
        {Ref, eos} ->
            %% receiving 'eos' tells us that we need to return a result.
            {ok, #{elapsed_time => erlang:system_time(1) - T0,
                   point_count => length(Points),
                   feature_count => count_features(Points),
                   distance => distance(Points)}, GrpcStream};
        {Ref, Point} ->
            record_route(Ref, Data#{acc => [Point | Points]}, GrpcStream)
    end.

-spec route_chat(reference(), GrpcStream :: grpcbox_stream:t()) -> ok.
route_chat(Ref, GrpcStream) ->
    route_chat(Ref, [], GrpcStream).

route_chat(Ref, Data, GrpcStream) ->
    receive
        {Ref, eos} ->
            ok;
        {Ref, #{location := Location} = P} ->
            Messages = proplists:get_all_values(Location, Data),
            [grpcbox_stream:send(Message, GrpcStream) || Message <- Messages],
            route_chat(Ref, [{Location, P} | Data], GrpcStream)
    end.

-spec generate_error(Ctx :: ctx:ctx(), Message :: map()) -> grpcbox_stream:grpc_extended_error_response().
generate_error(_Ctx, _Message) ->
    {
        grpc_extended_error, #{
            status => ?GRPC_STATUS_INTERNAL,
            message => <<"error_message">>,
            trailers => #{
                <<"generate_error_trailer">> => <<"error_trailer">>
            }
        }
    }.

-spec streaming_generate_error(Message :: map(), GrpcStream :: grpcbox_stream:t()) -> no_return().
streaming_generate_error(_Message, _GrpcStream) ->
    exit(
        {
            grpc_extended_error, #{
                status => ?GRPC_STATUS_INTERNAL,
                message => <<"error_message">>,
                trailers => #{
                    <<"generate_error_trailer">> => <<"error_trailer">>
                }
            }
        }
    ).

%% Supporting functions

data() ->
    case file:read_file("test/otel_interceptor_SUITE_data/route_guide_db.json") of
        {ok, Json} ->
            jsx:decode(Json, [return_maps, {labels, atom}]);
        {error, enoent} ->
            {ok, Json} = file:read_file("../../../../test/otel_interceptor_SUITE_data/route_guide_db.json"),
            jsx:decode(Json, [return_maps, {labels, atom}])
    end.

find_point(_Location, []) ->
    "";
find_point(Location, [#{location := Location, name := Name} | _]) ->
    Name;
find_point(Location, [_ | T]) ->
    find_point(Location, T).

count_features(_) -> 42.
distance(_) -> 42.
