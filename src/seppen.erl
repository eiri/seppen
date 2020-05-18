-module(seppen).

-behaviour(application).
-behaviour(supervisor).

-type key() :: binary().
-type value() :: binary().

%% public API
-export([set/2, get/1, hmac/1, member/1, delete/1, keys/0, keys/1]).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([start_link/0, init/1]).

-define(INDEX, seppen_index).
-define(STORE, seppen_store).


%% Public API

-spec set(key(), value()) -> ok | {error, term()}.
set(Key, Value) ->
    maybe_delete_old_hmac(Key),
    VHmac = seppen_hash:hmac(Value),
    case keys(VHmac) of
        [] ->
            Nodes = seppen_dispatch:shards(VHmac),
            gen_server:multi_call(Nodes, ?STORE, {set, VHmac, Value});
        _ ->
            ok
    end,
    AllNodes = seppen_dispatch:all_shards(),
    gen_server:multi_call(AllNodes, ?INDEX, {set, Key, VHmac}),
    ok.

-spec get(key()) -> {ok, value()} | {error, term()}.
get(Key) ->
    case hmac(Key) of
        {ok, VHmac} ->
            Nodes = seppen_dispatch:shards(VHmac),
            case rpc:multicall(Nodes, seppen_store, get, [?STORE, VHmac]) of
                {Replies, []} ->
                    [Reply] = sets:to_list(sets:from_list(Replies)),
                    Reply;
                {_, _BadNodes} ->
                    {error, rpc_fail}
            end;
        Error ->
            Error
    end.

-spec hmac(key()) -> {ok, binary()} | {error, term()}.
hmac(Key) ->
    seppen_store:get(?INDEX, Key).

-spec member(key()) -> boolean().
member(Key) ->
    seppen_store:member(?INDEX, Key).

-spec delete(key()) -> ok | {error, term()}.
delete(Key) ->
    case maybe_delete_old_hmac(Key) of
        ok ->
            AllNodes = seppen_dispatch:all_shards(),
            gen_server:multi_call(AllNodes, ?INDEX, {delete, Key}),
            ok;
        Error ->
            Error
    end.

-spec keys() -> [key()].
keys() ->
    seppen_store:keys(?INDEX).

-spec keys(Value :: value()) -> [key()].
keys(Value) ->
    seppen_store:keys(?INDEX, Value).


%% priv

maybe_delete_old_hmac(Key) ->
    case hmac(Key) of
        {ok, OldVHmac} ->
            case keys(OldVHmac) of
                [Key] ->
                    OldNodes = seppen_dispatch:shards(OldVHmac),
                    gen_server:abcast(OldNodes, ?STORE, {delete, OldVHmac}),
                    ok;
                _ ->
                    ok
            end;
        Error ->
            Error
    end.


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
