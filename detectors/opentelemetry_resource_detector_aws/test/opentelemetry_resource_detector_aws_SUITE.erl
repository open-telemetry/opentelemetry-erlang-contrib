-module(opentelemetry_resource_detector_aws_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/cloud_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/container_attributes.hrl").
-include_lib("opentelemetry_semantic_conventions/include/incubating/attributes/aws_attributes.hrl").

-define(CGROUP_DATA, <<"12:rdma:/
11:perf_event:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
10:freezer:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
9:memory:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
8:cpuset:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
7:devices:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
6:net_cls,net_prio:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
5:hugetlb:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
4:pids:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
3:cpu,cpuacct:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
2:blkio:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
1:name=systemd:/docker/a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81
0::/system.slice/containerd.service">>).

-define(CONTAINER_METADATA, "{
    \"DockerId\": \"ea32192c8553fbff06c9340478a2ff089b2bb5646fb718b4ee206641c9086d66\",
    \"Name\": \"curl\",
    \"DockerName\": \"ecs-curltest-24-curl-cca48e8dcadd97805600\",
    \"Image\": \"111122223333.dkr.ecr.us-west-2.amazonaws.com/curltest:latest\",
    \"ImageID\": \"sha256:d691691e9652791a60114e67b365688d20d19940dde7c4736ea30e660d8d3553\",
    \"Labels\": {
        \"com.amazonaws.ecs.cluster\": \"default\",
        \"com.amazonaws.ecs.container-name\": \"curl\",
        \"com.amazonaws.ecs.task-arn\": \"arn:aws:ecs:us-west-2:111122223333:task/default/8f03e41243824aea923aca126495f665\",
        \"com.amazonaws.ecs.task-definition-family\": \"curltest\",
        \"com.amazonaws.ecs.task-definition-version\": \"24\"
    },
    \"DesiredStatus\": \"RUNNING\",
    \"KnownStatus\": \"RUNNING\",
    \"Limits\": {
        \"CPU\": 10,
        \"Memory\": 128
    },
    \"CreatedAt\": \"2020-10-02T00:15:07.620912337Z\",
    \"StartedAt\": \"2020-10-02T00:15:08.062559351Z\",
    \"Type\": \"NORMAL\",
    \"LogDriver\": \"awslogs\",
    \"LogOptions\": {
        \"awslogs-create-group\": \"true\",
        \"awslogs-group\": \"/ecs/metadata\",
        \"awslogs-region\": \"us-west-2\",
        \"awslogs-stream\": \"ecs/curl/8f03e41243824aea923aca126495f665\"
    },
    \"ContainerARN\": \"arn:aws:ecs:us-west-2:111122223333:container/0206b271-b33f-47ab-86c6-a0ba208a70a9\",
    \"Networks\": [
        {
            \"NetworkMode\": \"awsvpc\",
            \"IPv4Addresses\": [
                \"10.0.2.100\"
            ],
            \"AttachmentIndex\": 0,
            \"MACAddress\": \"0e:9e:32:c7:48:85\",
            \"IPv4SubnetCIDRBlock\": \"10.0.2.0/24\",
            \"PrivateDNSName\": \"ip-10-0-2-100.us-west-2.compute.internal\",
            \"SubnetGatewayIpv4Address\": \"10.0.2.1/24\"
        }
    ]
}").

-define(TASK_METADATA, "{
    \"Cluster\": \"default\",
    \"TaskARN\": \"arn:aws:ecs:us-west-2:111122223333:task/default/158d1c8083dd49d6b527399fd6414f5c\",
    \"Family\": \"curltest\",
    \"ServiceName\": \"MyService\",
    \"Revision\": \"26\",
    \"DesiredStatus\": \"RUNNING\",
    \"KnownStatus\": \"RUNNING\",
    \"PullStartedAt\": \"2020-10-02T00:43:06.202617438Z\",
    \"PullStoppedAt\": \"2020-10-02T00:43:06.31288465Z\",
    \"AvailabilityZone\": \"us-west-2d\",
    \"VPCID\": \"vpc-1234567890abcdef0\",
    \"LaunchType\": \"EC2\",
    \"Containers\": [
        {
            \"DockerId\": \"598cba581fe3f939459eaba1e071d5c93bb2c49b7d1ba7db6bb19deeb70d8e38\",
            \"Name\": \"~internal~ecs~pause\",
            \"DockerName\": \"ecs-curltest-26-internalecspause-e292d586b6f9dade4a00\",
            \"Image\": \"amazon/amazon-ecs-pause:0.1.0\",
            \"ImageID\": \"\",
            \"Labels\": {
                \"com.amazonaws.ecs.cluster\": \"default\",
                \"com.amazonaws.ecs.container-name\": \"~internal~ecs~pause\",
                \"com.amazonaws.ecs.task-arn\": \"arn:aws:ecs:us-west-2:111122223333:task/default/158d1c8083dd49d6b527399fd6414f5c\",
                \"com.amazonaws.ecs.task-definition-family\": \"curltest\",
                \"com.amazonaws.ecs.task-definition-version\": \"26\"
            },
            \"DesiredStatus\": \"RESOURCES_PROVISIONED\",
            \"KnownStatus\": \"RESOURCES_PROVISIONED\",
            \"Limits\": {
                \"CPU\": 0,
                \"Memory\": 0
            },
            \"CreatedAt\": \"2020-10-02T00:43:05.602352471Z\",
            \"StartedAt\": \"2020-10-02T00:43:06.076707576Z\",
            \"Type\": \"CNI_PAUSE\",
            \"Networks\": [
                {
                    \"NetworkMode\": \"awsvpc\",
                    \"IPv4Addresses\": [
                        \"10.0.2.61\"
                    ],
                    \"AttachmentIndex\": 0,
                    \"MACAddress\": \"0e:10:e2:01:bd:91\",
                    \"IPv4SubnetCIDRBlock\": \"10.0.2.0/24\",
                    \"PrivateDNSName\": \"ip-10-0-2-61.us-west-2.compute.internal\",
                    \"SubnetGatewayIpv4Address\": \"10.0.2.1/24\"
                }
            ]
        },
        {
            \"DockerId\": \"ee08638adaaf009d78c248913f629e38299471d45fe7dc944d1039077e3424ca\",
            \"Name\": \"curl\",
            \"DockerName\": \"ecs-curltest-26-curl-a0e7dba5aca6d8cb2e00\",
            \"Image\": \"111122223333.dkr.ecr.us-west-2.amazonaws.com/curltest:latest\",
            \"ImageID\": \"sha256:d691691e9652791a60114e67b365688d20d19940dde7c4736ea30e660d8d3553\",
            \"Labels\": {
                \"com.amazonaws.ecs.cluster\": \"default\",
                \"com.amazonaws.ecs.container-name\": \"curl\",
                \"com.amazonaws.ecs.task-arn\": \"arn:aws:ecs:us-west-2:111122223333:task/default/158d1c8083dd49d6b527399fd6414f5c\",
                \"com.amazonaws.ecs.task-definition-family\": \"curltest\",
                \"com.amazonaws.ecs.task-definition-version\": \"26\"
            },
            \"DesiredStatus\": \"RUNNING\",
            \"KnownStatus\": \"RUNNING\",
            \"Limits\": {
                \"CPU\": 10,
                \"Memory\": 128
            },
            \"CreatedAt\": \"2020-10-02T00:43:06.326590752Z\",
            \"StartedAt\": \"2020-10-02T00:43:06.767535449Z\",
            \"Type\": \"NORMAL\",
            \"LogDriver\": \"awslogs\",
            \"LogOptions\": {
                \"awslogs-create-group\": \"true\",
                \"awslogs-group\": \"/ecs/metadata\",
                \"awslogs-region\": \"us-west-2\",
                \"awslogs-stream\": \"ecs/curl/158d1c8083dd49d6b527399fd6414f5c\"
            },
            \"ContainerARN\": \"arn:aws:ecs:us-west-2:111122223333:container/abb51bdd-11b4-467f-8f6c-adcfe1fe059d\",
            \"Networks\": [
                {
                    \"NetworkMode\": \"awsvpc\",
                    \"IPv4Addresses\": [
                        \"10.0.2.61\"
                    ],
                    \"AttachmentIndex\": 0,
                    \"MACAddress\": \"0e:10:e2:01:bd:91\",
                    \"IPv4SubnetCIDRBlock\": \"10.0.2.0/24\",
                    \"PrivateDNSName\": \"ip-10-0-2-61.us-west-2.compute.internal\",
                    \"SubnetGatewayIpv4Address\": \"10.0.2.1/24\"
                }
            ]
        }
    ]
}").

all() ->
    [
        json_request_with_httpc_error,
        json_request_when_httpc_doesnt_have_200_status,
        json_request_with_invalid_json,
        json_request_with_valid_json,
        ecs_get_resource_when_env_not_set,
        ecs_get_resource_with_invalid_cgroup_file,
        ecs_get_resource_with_invalid_container_metadata,
        ecs_get_resource_with_invalid_task_metadata,
        ecs_get_resource_should_return_ecs_metadata
    ].

init_per_suite(Config) ->
    application:ensure_all_started(opentelemetry_resource_detector_aws),
    Config.

end_per_suite(_Config) ->
    application:stop(opentelemetry_resource_detector_aws),
    ok.

ecs_get_resource_when_env_not_set(_Config) ->
    try otel_resource_aws_ecs:get_resource([]) of
        _ -> ct:fail(failed_to_catch_exception)
    catch
        _Class:Reason -> ?assertEqual('Not running in ECS environment, ECS_CONTAINER_METADATA_URI_V4 not set.', Reason)
    end.

json_request_with_httpc_error(_Config) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(get, _, _, _) -> {error, invalid} end),

    ?assertEqual({error, invalid}, opentelemetry_resource_detector_aws:json_request(get, "http://localhost")),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

json_request_when_httpc_doesnt_have_200_status(_Config) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(get, _, _, _) -> {ok, {{"1.1", 401, ""}, [], <<>>}} end),

    ?assertEqual({error, {invalid_status_code, 401}}, opentelemetry_resource_detector_aws:json_request(get, "http://localhost")),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

json_request_with_invalid_json(_Config) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(get, _, _, _) -> {ok, {{"1.1", 200, ""}, [], "-"}} end),

    ?assertEqual({error, {invalid_json, unexpected_end}}, opentelemetry_resource_detector_aws:json_request(get, "http://localhost")),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

json_request_with_valid_json(_Config) ->
    meck:new(httpc),
    meck:expect(httpc, request, fun(get, {_URL, Headers}, _, _) ->
        {_, UserAgent} = lists:keyfind("User-Agent", 1, Headers),
        {ok, ExporterVsn} = application:get_key(opentelemetry_resource_detector_aws, vsn),
        ExpectedUserAgent = lists:flatten(io_lib:format("OTel-Resource-Detector-AWS-erlang/~s", [ExporterVsn])),

        ?assertEqual(ExpectedUserAgent, UserAgent),

        {ok, {{"1.1", 200, ""}, [], "{}"}}
    end),

    ?assertEqual({ok, #{}}, opentelemetry_resource_detector_aws:json_request(get, "http://localhost")),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).
    
ecs_get_resource_with_invalid_cgroup_file(_Config) ->
    os:putenv("ECS_CONTAINER_METADATA_URI_V4", "http://localhost"),

    meck:new(httpc),
    meck:expect(httpc, request, fun
        (get, {"http://localhost", _Headers}, _, _) -> {ok, {{"1.1", 200, ""}, [], ?CONTAINER_METADATA}};
        (get, {"http://localhost/task", _Headers}, _, _) -> {ok, {{"1.1", 200, ""}, [], ?TASK_METADATA}}
    end),

    Resource = otel_resource_aws_ecs:get_resource([{cgroup_path, "/path/to/invalid/file"}]),

    ?assertNot(maps:is_key(?CONTAINER_ID, otel_attributes:map(otel_resource:attributes(Resource)))),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

ecs_get_resource_with_invalid_container_metadata(_Config) ->
    os:putenv("ECS_CONTAINER_METADATA_URI_V4", "http://localhost"),
    Path = create_dummy_cgroup_file(),

    meck:new(httpc),
    meck:expect(httpc, request, fun(get, {_URL, _Headers}, _, _) -> {ok, {{"1.1", 500, ""}, [], <<>>}} end),

    Resource = otel_resource_aws_ecs:get_resource([{cgroup_path, Path}]),

    ?assertNot(maps:is_key(?AWS_ECS_CONTAINER_ARN, otel_attributes:map(otel_resource:attributes(Resource)))),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

ecs_get_resource_with_invalid_task_metadata(_Config) ->
    os:putenv("ECS_CONTAINER_METADATA_URI_V4", "http://localhost"),
    Path = create_dummy_cgroup_file(),

    meck:new(httpc),
    meck:expect(httpc, request, fun
        (get, {"http://localhost", _Headers}, _, _) -> {ok, {{"1.1", 200, ""}, [], ?CONTAINER_METADATA}};
        (get, {"http://localhost/task", _Headers}, _, _) -> {ok, {{"1.1", 500, ""}, [], <<>>}}
    end),

    Resource = otel_resource_aws_ecs:get_resource([{cgroup_path, Path}]),

    ?assertNot(maps:is_key(?AWS_ECS_CONTAINER_ARN, otel_attributes:map(otel_resource:attributes(Resource)))),

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

ecs_get_resource_should_return_ecs_metadata(_Config) ->
    os:putenv("ECS_CONTAINER_METADATA_URI_V4", "http://localhost"),
    Path = create_dummy_cgroup_file(),
    {ok, Hostname} = inet:gethostname(),

    meck:new(httpc),
    meck:expect(httpc, request, fun
        (get, {"http://localhost", _Headers}, _, _) -> {ok, {{"1.1", 200, ""}, [], ?CONTAINER_METADATA}};
        (get, {"http://localhost/task", _Headers}, _, _) -> {ok, {{"1.1", 200, ""}, [], ?TASK_METADATA}}
    end),
    Resource = otel_resource_aws_ecs:get_resource([{cgroup_path, Path}]),

    ?assertEqual(
        #{
            ?CLOUD_PROVIDER => <<"aws">>,
            ?CLOUD_PLATFORM => <<"aws_ecs">>,
            ?CONTAINER_NAME => binary:list_to_bin(Hostname),
            ?CONTAINER_ID => <<"a2ffe0e97ac22657a2a023ad628e9df837c38a03b1ebc904d3f6d644eb1a1a81">>,
            ?AWS_ECS_CONTAINER_ARN => <<"arn:aws:ecs:us-west-2:111122223333:container/0206b271-b33f-47ab-86c6-a0ba208a70a9">>,
            ?AWS_ECS_CLUSTER_ARN => <<"arn:aws:ecs:us-west-2:111122223333:cluster/default">>,
            ?AWS_ECS_LAUNCHTYPE => <<"ec2">>,
            ?AWS_ECS_TASK_ARN => <<"arn:aws:ecs:us-west-2:111122223333:task/default/158d1c8083dd49d6b527399fd6414f5c">>,
            ?AWS_ECS_TASK_FAMILY => <<"curltest">>,
            ?AWS_ECS_TASK_REVISION => <<"26">>,
            ?CLOUD_ACCOUNT_ID => <<"111122223333">>,
            ?CLOUD_REGION => <<"us-west-2">>,
            ?CLOUD_RESOURCE_ID => <<"arn:aws:ecs:us-west-2:111122223333:container/0206b271-b33f-47ab-86c6-a0ba208a70a9">>,
            ?CLOUD_AVAILABILITY_ZONE => <<"us-west-2d">>,
            ?AWS_LOG_GROUP_NAMES => <<"/ecs/metadata">>,
            ?AWS_LOG_GROUP_ARNS => <<"arn:aws:logs:us-west-2:111122223333:log-group:/ecs/metadata">>,
            ?AWS_LOG_STREAM_NAMES => <<"ecs/curl/8f03e41243824aea923aca126495f665">>,
            ?AWS_LOG_STREAM_ARNS => <<"arn:aws:logs:us-west-2:111122223333:log-group:/ecs/metadata:log-stream:ecs/curl/8f03e41243824aea923aca126495f665">>
        },
        otel_attributes:map(otel_resource:attributes(Resource))),
    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

create_dummy_cgroup_file() ->
    {ok, Path} = mktemp("opentelemetry_resource_detector_aws_SUITE"),
    file:write_file(Path, ?CGROUP_DATA),
    Path.

mktemp(Prefix) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
    TempDir = filename:basedir(user_cache, Prefix),
    TempFile = filename:join(TempDir, Rand),
    []= os:cmd("mkdir -p " ++ "\"" ++ TempDir ++ "\""),
    {ok, _} = file:list_dir(TempDir),
    Result = file:write_file(TempFile, <<>>),
    case {Result} of
            {ok}    -> {ok, TempFile};
            {Error}  -> Error
    end.

http_body(URL) ->
    case URL of
        "metadata_uri_v4" -> ?CONTAINER_METADATA;
        "metadata_uri_v4/task" -> ?TASK_METADATA
    end.
