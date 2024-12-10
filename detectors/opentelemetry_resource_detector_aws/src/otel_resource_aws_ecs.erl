-module(otel_resource_aws_ecs).

-behavior(otel_resource_detector).

-export([get_resource/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/cloud_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/container_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/aws_attributes.hrl").

-define(OS_ENV, "ECS_CONTAINER_METADATA_URI_V4").
-define(DEFAULT_CGROUP_PATH, "/proc/self/cgroup").

%% @private
-spec get_resource(list()) -> otel_resource:t() | no_return().
get_resource(Config) ->
    otel_resource:create(fetch(Config)).

fetch(Config) ->
    case os:getenv(?OS_ENV) of
        false ->
          erlang:error('Not running in ECS environment, ECS_CONTAINER_METADATA_URI_V4 not set.');
        _MetadataV4URL ->
          [
            {?CLOUD_PROVIDER, atom_to_list(?CLOUD_PROVIDER_VALUES_AWS)},
            {?CLOUD_PLATFORM, atom_to_list(?CLOUD_PLATFORM_VALUES_AWS_ECS)} |
            aws_ecs_attributes(Config)
          ]
    end.

aws_ecs_attributes(Config) ->
  {ok, Hostname} = inet:gethostname(),

  ContainerIDAttributes =
    case fetch_container_id_attributes(Config) of
      {ok, ContainerIDAttributesResult} ->
        ContainerIDAttributesResult;
      {error, ContainerIDAttributesError} ->
        ?LOG_ERROR("Failed to fetch Container ID attributes: ~s", [ContainerIDAttributesError]),
        []
    end,
  
  MetadataV4Attributes =
    case fetch_metadata_v4_attributes() of
      {ok, MetadataV4AttributesResult} ->
        MetadataV4AttributesResult;
      {error, MetadataV4AttributesError} ->
        ?LOG_ERROR("Failed to fetch metadata attributes: ~s", [MetadataV4AttributesError]),
        []
    end,
  
  [{?CONTAINER_NAME, Hostname}] ++ ContainerIDAttributes ++ MetadataV4Attributes.

fetch_container_id_attributes(Config) ->
  Path = proplists:get_value(cgroup_path, Config, ?DEFAULT_CGROUP_PATH),
  
  case file:read_file(Path) of
      {ok, Data} ->
          Lines = binary:split(Data, <<"\n">>, [global]),
          ContainerId = lists:foldl(fun
            (Line, none) ->
              leength,
              if byte_size(Line) > 64 ->
                {ok, binary:part(Line, byte_size(Line) - 64, 64)};
              true ->
                  none
              end;

            (_Line, Acc) ->
              Acc
          end, none, Lines),
          case ContainerId of
              none -> {error, "Container ID not found in " ++ Path};
              {ok, ContainerIdValue} -> {ok, [{?CONTAINER_ID, ContainerIdValue}]}
          end;
      {error, Error} ->
          {error, "Failed to read " ++ Path ++ ": " ++ io_lib:format("~p", [Error])}
  end.

fetch_metadata_v4_attributes() ->
  ContainerURL = os:getenv(?OS_ENV),
  TaskURL = ContainerURL ++ "/task",
  case opentelemetry_resource_detector_aws:json_request(get, ContainerURL) of
    {ok, ContainerMetadata} ->
      case opentelemetry_resource_detector_aws:json_request(get, TaskURL) of
        {ok, TaskMetadata} ->
          ContainerARN = maps:get(<<"ContainerARN">>, ContainerMetadata),
          [_, _, _, Region, AccountId | _Resource] = binary:split(ContainerARN, <<":">>, [global]),
          TaskARN = maps:get(<<"TaskARN">>, TaskMetadata),
          ClusterARNOrShortName = maps:get(<<"Cluster">>, TaskMetadata),
          ClusterARN =
            case ClusterARNOrShortName of
                <<"arn:", _Rest/binary>> -> ClusterARNOrShortName;
                <<_Any/binary>> -> <<"arn:aws:ecs:", Region/binary, ":", AccountId/binary, ":cluster/", ClusterARNOrShortName/binary>>
            end,
          LaunchType = binary:list_to_bin(string:to_lower(binary:bin_to_list(maps:get(<<"LaunchType">>, TaskMetadata)))),

          Attributes =
            [
              {?AWS_ECS_CONTAINER_ARN, ContainerARN},
              {?AWS_ECS_CLUSTER_ARN, ClusterARN},
              {?AWS_ECS_LAUNCHTYPE, LaunchType},
              {?AWS_ECS_TASK_ARN, TaskARN},
              {?AWS_ECS_TASK_FAMILY, maps:get(<<"Family">>, TaskMetadata)},
              {?AWS_ECS_TASK_REVISION, maps:get(<<"Revision">>, TaskMetadata)},
              {?CLOUD_ACCOUNT_ID, AccountId},
              {?CLOUD_REGION, Region},
              {?CLOUD_RESOURCE_ID, ContainerARN}
            ] ++ case maps:find(<<"AvailabilityZone">>, TaskMetadata) of
              {ok, AZ} -> [{?CLOUD_AVAILABILITY_ZONE, AZ}];
              error -> []
            end ++ case {maps:get(<<"LogDriver">>, ContainerMetadata), maps:is_key(<<"LogOptions">>, ContainerMetadata)} of
              {<<"awslogs">>, true} ->
                  LogOptions = maps:get(<<"LogOptions">>, ContainerMetadata),
                  Region = maps:get(<<"awslogs-region">>, LogOptions, Region),
                  GroupName = maps:get(<<"awslogs-group">>, LogOptions),
                  GroupARN = <<"arn:aws:logs:", Region/binary, ":", AccountId/binary, ":log-group:", GroupName/binary>>,
                  StreamName = maps:get(<<"awslogs-stream">>, LogOptions),
                  StreamARN = <<GroupARN/binary, ":log-stream:", StreamName/binary>>,
      
                  [
                      {?AWS_LOG_GROUP_NAMES, [GroupName]},
                      {?AWS_LOG_GROUP_ARNS, [GroupARN]},
                      {?AWS_LOG_STREAM_NAMES, [StreamName]},
                      {?AWS_LOG_STREAM_ARNS, [StreamARN]}
                  ];
              {_, _} ->
                  []
            end,

          {ok, Attributes};

        {error, Error} ->
          {error, Error}
      end;

    {error, Error} ->
      {error, Error}
  end.
