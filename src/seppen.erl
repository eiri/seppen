-module(seppen).

-behaviour(application).
-behaviour(supervisor).

-type key() :: binary().
-type value() :: binary().

%% public API
-export([list/0, get/1, member/1, set/2, delete/1]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).

-define(STORE, seppen_store).


%% Public API

-spec list() -> [key()].
list() ->
    Keys = gen_server:call(?STORE, list),
    lists:sort(Keys).

-spec get(key()) -> {ok, value()} | {error, term()}.
get(Key) ->
    gen_server:call(?STORE, {get, Key}).

-spec member(key()) -> boolean().
member(Key) ->
    gen_server:call(?STORE, {member, Key}).

-spec set(key(), value()) -> ok | {error, term()}.
set(Key, Value) ->
    gen_server:call(?STORE, {set, Key, Value}).

-spec delete(key()) -> ok | {error, term()}.
delete(Key) ->
    gen_server:call(?STORE, {delete, Key}).


%% application callbacks

start(_Type, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/:key", seppen_rest, []}]}
    ]),
    TransportOpts = [{port, 21285}],
    ProtocolOpts = #{
        env => #{dispatch => Dispatch}
    },
    {ok, _} = cowboy:start_clear(http, TransportOpts, ProtocolOpts),
    seppen:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
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
