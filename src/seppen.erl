-module(seppen).

-behaviour(application).
-behaviour(supervisor).

-type key() :: binary().
-type value() :: binary().

%% public API
-export([set/2, get/1, hmac/1, member/1, delete/1, list/0]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).

-define(INDEX, seppen_index).
-define(STORE, seppen_store).


%% Public API

-spec set(key(), value()) -> ok | {error, term()}.
set(Key, Value) ->
    case gen_server:call(?INDEX, {get, Key}) of
        {ok, OldVHmac} ->
            %% FIXME! check that none other keys point to this before deleting
            OldNodes = seppen_dispatch:shards(OldVHmac),
            abcast = gen_server:abcast(OldNodes, ?STORE, {delete, OldVHmac});
        {error, not_found} ->
            ok
    end,
    VHmac = seppen_hash:hmac(Value),
    Nodes = seppen_dispatch:shards(VHmac),
    AllNodes = seppen_dispatch:all_shards(),
    abcast = gen_server:abcast(Nodes, ?STORE, {set, VHmac, Value}),
    abcast = gen_server:abcast(AllNodes, ?INDEX, {set, Key, VHmac}),
    ok.

-spec get(key()) -> {ok, value()} | {error, term()}.
get(Key) ->
    case gen_server:call(?INDEX, {get, Key}) of
        {ok, VHmac} ->
            Nodes = seppen_dispatch:shards(VHmac),
            {Replies, []} = gen_server:multi_call(Nodes, ?STORE, {get, VHmac}),
            {_, Reply} = hd(Replies),
            Reply;
        Error ->
            Error
    end.

-spec hmac(key()) -> {ok, binary()} | {error, term()}.
hmac(Key) ->
    gen_server:call(?INDEX, {get, Key}).

-spec member(key()) -> boolean().
member(Key) ->
    gen_server:call(?INDEX, {member, Key}).

-spec delete(key()) -> ok | {error, term()}.
delete(Key) ->
    case gen_server:call(?INDEX, {get, Key}) of
        {ok, OldVHmac} ->
            OldNodes = seppen_dispatch:shards(OldVHmac),
            abcast = gen_server:abcast(OldNodes, ?STORE, {delete, OldVHmac}),
            AllNodes = seppen_dispatch:all_shards(),
            abcast = gen_server:abcast(AllNodes, ?INDEX, {delete, Key}),
            ok;
        Error ->
            Error
    end.

-spec list() -> [key()].
list() ->
    gen_server:call(?INDEX, list).


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
            start => {seppen_store, start_link, [?STORE]}
        },
        #{
            id => seppen_index,
            start => {seppen_store, start_link, [?INDEX]}
        },
        #{
            id => seppen_dispatch,
            start => {seppen_dispatch, start_link, []}
        },
        #{
            id => seppen_rest,
            start => {seppen_rest, start_link, []}
        }

    ],
    {ok, {#{}, Children}}.
