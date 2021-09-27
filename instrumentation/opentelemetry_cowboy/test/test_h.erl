-module(test_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(_, failure) ->
    error(failure);
init(Req, success = Opts) ->
    {ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Opts};
init(Req, slow = Opts) ->
    timer:sleep(200),
    {ok, cowboy_req:reply(200, #{}, <<"I'm slow">>, Req), Opts};
init(Req0, chunked = Opts) ->
    Req = cowboy_req:stream_reply(200, Req0),
    cowboy_req:stream_body("Hello\r\n", nofin, Req),
    cowboy_req:stream_body("World\r\n", fin, Req),
    {ok, Req, Opts};
init(Req0, chunked_slow = Opts) ->
    Req = cowboy_req:stream_reply(200, Req0),
    cowboy_req:stream_body("Hello\r\n", nofin, Req),
    timer:sleep(200),
    cowboy_req:stream_body("World\r\n", fin, Req),
    {ok, Req, Opts}.
