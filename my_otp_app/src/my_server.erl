-module(my_server).
-behaviour(gen_server).

%% API
-export([start_link/0, get_value/0, set_value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Start the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% API functions
get_value() ->
    gen_server:call(?MODULE, get_value).

set_value(Value) ->
    gen_server:cast(?MODULE, {set_value, Value}).

%% gen_server callbacks
init([]) ->
    {ok, #{value => 0}}.

handle_call(get_value, _From, State) ->
    {reply, maps:get(value, State), State}.

handle_cast({set_value, Value}, State) ->
    {noreply, maps:put(value, Value, State)}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
