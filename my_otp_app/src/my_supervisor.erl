-module(my_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor init
init([]) ->
    %% Define child specs
    ChildSpecs = [
        {my_server, {my_server, start_link, []}, permanent, 5000, worker, [my_server]}
    ],

    %% Define supervision strategy
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
