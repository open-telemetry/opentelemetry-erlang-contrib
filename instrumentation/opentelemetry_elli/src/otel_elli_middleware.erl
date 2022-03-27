%%%------------------------------------------------------------------------
%% Copyright 2020, Tristan Sloughter
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
%% @doc Elli middleware for tracing requests and recording stats.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_elli_middleware).

-export([preprocess/2,
         handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-define(EXCLUDED_URLS, {?MODULE, excluded_urls}).

preprocess(Req, _) ->
    %% extract trace context from headers to be used as the parent
    Headers = elli_request:headers(Req),
    otel_propagator_text_map:extract(Headers),
    case lists:member(elli_request:raw_path(Req), persistent_term:get(?EXCLUDED_URLS, [])) of
        true ->
            Req;
        false ->
            otel_elli:start_span(Req),
            Req
    end.

handle(_Req, _Config) ->
    ignore.

handle_event(elli_startup, _Args, _Config) ->
    ExcludedUrls = collect_excluded_urls(),

    %% TODO: This should really be per-server and not global across what could be multiple Elli
    %% servers running within a node
    persistent_term:put(?EXCLUDED_URLS, ExcludedUrls),

    ok;
handle_event(request_complete, Args, Config) ->
    handle_full_response(request_complete, Args, Config);
handle_event(chunk_complete, Args, Config) ->
    handle_full_response(chunk_complete, Args, Config);

handle_event(request_timeout, _, _Config) ->
    handle_exception(request_timeout);
handle_event(request_parse_error, [Reason], _Config) ->
    handle_exception({request_parse_error, Reason});
handle_event(client_closed, [RequestPart], _Config) ->
    handle_exception({client_closed, RequestPart});
handle_event(client_timeout, [RequestPart], _Config) ->
    handle_exception({client_timeout, RequestPart});
handle_event(bad_request, [Reason], _Config) ->
    handle_exception({bad_request, Reason});
handle_event(request_error, [_Req, Exception, Stacktrace], _Config) ->
    handle_exception(error, Exception, Stacktrace);
handle_event(request_throw, [_Req, Exception, Stacktrace], _Config) ->
    handle_exception(throw, Exception, Stacktrace);
handle_event(request_exit, [_Req, Exception, Stacktrace], _Config) ->
    handle_exception(exit, Exception, Stacktrace);
handle_event(invalid_return, [_Req, _Unexpected], _Config) ->
    %% TODO: should we include the `Unexpected' response in the attributes?
    %% it could have sensitive data so should be configurable
    handle_exception(invalid_return),
    ok;
handle_event(_Event, _Args, _Config) ->
    ok.

%%

handle_full_response(_Type, [_Req, Code, _Hs, _B, {_Timings, Sizes}], _Config) ->
    maybe_set_req_body_size(Sizes),
    maybe_set_resp_body_size(Sizes),
    set_status(Code),

    %% end the span that the user might have started
    %% if there is no started span this is a noop
    ?end_span(),

    %% `end_span' does not change the active span context in the context
    %% so here the whole context must be cleared or it'd be carried over
    %% to the next request that runs in the same process.
    otel_ctx:clear(),

    ok.

set_status(Code) ->
    Status = opentelemetry:status(http_to_otel_status(Code), <<>>),
    ?set_status(Status),
    ?set_attribute(<<"http.status">>, Code).

%% Elli doesn't support any decompression so it would be up to the
%% user or their middleware/handover handler to set the attribute
%% `http.request_content_length_uncompressed'
maybe_set_req_body_size(Sizes) ->
    case proplists:get_value(req_body, Sizes) of
        undefined ->
            ok;
        ReqBodySize ->
            ?set_attribute(<<"http.request_content_length">>, ReqBodySize)
    end.

%% Because Elli compression support is a middleware `elli_middleware_compress'
%% we are not able to set `http.request_content_length_uncompressed'.
%% So if the response body is compressed the `http.response_content_length'
%% will still be the uncompressed size.
maybe_set_resp_body_size(Sizes) ->
    case proplists:get_value(chunks, Sizes) of
        undefined ->
            case proplists:get_value(file, Sizes) of
                undefined ->
                    case proplists:get_value(resp_body, Sizes) of
                        undefined ->
                            ok;
                        RespBodySize ->
                            set_resp_body_size(RespBodySize)
                    end;
                FileSize ->
                    set_resp_body_size(FileSize)
            end;
        ChunksSize ->
            set_resp_body_size(ChunksSize)
    end.

set_resp_body_size(Size) ->
    ?set_attribute(<<"http.response_content_length">>, Size).

handle_exception(Reason) ->
    ?set_attributes([{<<"error.message">>, format_reason(Reason)}]).

handle_exception(Class, Reason, Stacktrace) ->
    ?set_attributes([{<<"stacktrace">>, format_exception(Class, Reason, Stacktrace)},
                     {<<"error.message">>, term_to_string(Reason)}]).

format_reason(Reason) ->
    term_to_string(Reason).

-if(?OTP_RELEASE >= 24).
format_exception(Class, Reason, StackTrace) ->
    erl_error:format_exception(Class, Reason, StackTrace).
-else.
format_exception(Class, Reason, StackTrace) ->
    io_lib:format("~p:~p ~p", [Class, Reason, StackTrace]).
-endif.

term_to_string(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

http_to_otel_status(Code) when Code >= 500 ->
    ?OTEL_STATUS_ERROR;
http_to_otel_status(_) ->
    ?OTEL_STATUS_UNSET.

collect_excluded_urls() ->
    %% support an app and os env var for setting paths to not trace
    OSExcludePaths = case os:getenv("OTEL_ELLI_EXCLUDED_PATHS") of
                         false ->
                             [];
                         E ->
                             string:split(E, ",", all)
                     end,

    AppExcludedPaths = application:get_env(opentelemetry_elli, excluded_paths, []),

    lists:umerge(sort_paths(AppExcludedPaths),
                 sort_paths(OSExcludePaths)).

sort_paths(Paths) ->
    lists:usort(lists:filtermap(fun path_to_binary/1, Paths)).

path_to_binary(Path) when is_list(Path) ; is_binary(Path) ->
    case unicode:characters_to_binary(Path) of
        {error, _, _} ->
            false;
        Binary ->
            {true, Binary}
    end.
