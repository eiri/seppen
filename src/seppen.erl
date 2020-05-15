-module(seppen).

-behaviour(application).
-behaviour(supervisor).

-type key() :: binary().
-type value() :: binary().

%% public API
-export([list/0, get/1, get_uid/1, member/1, set/2, delete/1]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).

-define(STORE, seppen_store).


%% Public API

-spec list() -> [key()].
list() ->
    Nodes = seppen_dispatch:all_shards(),
    {Replies, _} = gen_server:multi_call(Nodes, ?STORE, list),
    lists:merge([R || {_Node, R} <- Replies]).

-spec get(key()) -> {ok, value()} | {error, term()}.
get(Key) ->
    Nodes = seppen_dispatch:shards(Key),
    Reply = gen_server:multi_call(Nodes, ?STORE, {get, Key}),
    multi_reply(Reply).

-spec get_uid(key()) -> {ok, binary()} | {error, term()}.
get_uid(Key) ->
    Nodes = seppen_dispatch:shards(Key),
    Reply = gen_server:multi_call(Nodes, ?STORE, {get_uid, Key}),
    multi_reply(Reply).

-spec member(key()) -> boolean().
member(Key) ->
    Nodes = seppen_dispatch:shards(Key),
    Reply = gen_server:multi_call(Nodes, ?STORE, {member, Key}),
    multi_reply(Reply).

-spec set(key(), value()) -> ok | {error, term()}.
set(Key, Value) ->
    Nodes = seppen_dispatch:shards(Key),
    Reply = gen_server:multi_call(Nodes, ?STORE, {set, Key, Value}),
    multi_reply(Reply).

-spec delete(key()) -> ok | {error, term()}.
delete(Key) ->
    Nodes = seppen_dispatch:shards(Key),
    Reply = gen_server:multi_call(Nodes, ?STORE, {delete, Key}),
    multi_reply(Reply).

%% private

multi_reply({Replies, _BadNodes}) ->
    [Reply] = sets:to_list(sets:from_list([R || {_Node, R} <- Replies])),
    Reply.


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
        },
        #{
            id => seppen_dispatch,
            start => {seppen_dispatch, start_link, []}
        }
    ],
    {ok, {#{}, Children}}.
