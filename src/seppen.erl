-module(seppen).

-behaviour(application).
-behaviour(supervisor).

%% public API
-export([list/0, get/1, set/2, delete/1]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).


%% Public API

list() ->
    [].

get(_) ->
    {}.

set(_, _) ->
    ok.

delete(_) ->
    ok.


%% application callbacks

start(_Type, _StartArgs) ->
    seppen:start_link().

stop(_State) ->
    ok.


%% supervisor callbacks

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => seppen_store,
            start => {seppen_store, start_link, []}
        }
    ],
    {ok, {#{}, Children}}.
