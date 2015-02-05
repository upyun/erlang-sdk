-module(up_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Server = {up_core, {up_core, start_link, []},
              permanent, 2000, worker, [up_core]},
    Children = [Server],
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.
