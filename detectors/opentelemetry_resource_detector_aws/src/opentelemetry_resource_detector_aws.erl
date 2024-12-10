-module(opentelemetry_resource_detector_aws).

-export([json_request/2]).

json_request(Method, URL) ->
    case httpc:request(Method, {URL, [{"User-Agent", user_agent()}]}, [], []) of
      {ok, {{_, 200, _}, _, Body}} ->
        case json_decode(Body) of
          {ok, Data} -> {ok, Data};
          {error, Error} -> {error, {invalid_json, Error}}
        end;
      {ok, {{_, Code, _}, _, _}} -> {error, {invalid_status_code, Code}};
      {error, Error} -> {error, Error}
    end.

json_decode(String) ->
    try json:decode(list_to_binary(String)) of
        Data -> {ok, Data}
    catch
        _:Error -> {error, Error}
    end.

user_agent() ->
    {ok, Vsn} = application:get_key(opentelemetry_resource_detector_aws, vsn),
      lists:flatten(io_lib:format("OTel-Resource-Detector-AWS-erlang/~s", [Vsn])).
