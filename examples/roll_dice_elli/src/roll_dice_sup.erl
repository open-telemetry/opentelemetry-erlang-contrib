%%%-------------------------------------------------------------------
%% @doc roll_dice top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(roll_dice_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Port = list_to_integer(os:getenv("PORT", "3000")),

    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 10},

    ElliOpts = [{callback, elli_middleware},
                {callback_args, [{mods, [{otel_elli_middleware, []},
                                         {roll_dice_handler, []}]}]},
                {port, Port}],


    ChildSpecs = [#{id => roll_dice_http,
                    start => {elli, start_link, [ElliOpts]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [roll_dice_handler]}],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
