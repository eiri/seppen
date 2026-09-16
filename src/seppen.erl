-module(seppen).

-behaviour(application).
-behaviour(supervisor).

-type key() :: binary().
-type value() :: binary().

%% public API
-export([
    set/2,
    maybe_set/3,
    get/1,
    hmac/1,
    member/1,
    delete/1,
    maybe_delete/2,
    keys/0,
    keys/1
]).
%% application callbacks
-export([start/2, stop/1]).
%% supervisor callbacks
-export([start_link/0, init/1]).

-define(INDEX, seppen_index).
-define(STORE, seppen_store).

%% Public API

-spec set(key(), value()) -> ok | {error, term()}.
set(Key, Value) ->
    with_lock(Key, fun() -> set_value(Key, Value) end).

-spec maybe_set(key(), binary(), value()) -> ok | {error, term()}.
maybe_set(Key, ExpectedHmac, Value) ->
    with_lock(
        Key,
        fun() ->
            case hmac(Key) =:= {ok, ExpectedHmac} of
                true -> set_value(Key, Value);
                false -> {error, precondition_failed}
            end
        end
    ).

-spec get(key()) -> {ok, value()} | {error, term()}.
get(Key) ->
    case hmac(Key) of
        {ok, VHmac} ->
            get_value(VHmac);
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
    with_lock(Key, fun() -> delete_key(Key) end).

-spec maybe_delete(key(), binary()) -> ok | {error, term()}.
maybe_delete(Key, ExpectedHmac) ->
    with_lock(
        Key,
        fun() ->
            case hmac(Key) =:= {ok, ExpectedHmac} of
                true -> delete_key(Key);
                false -> {error, precondition_failed}
            end
        end
    ).

-spec keys() -> [key()].
keys() ->
    seppen_store:keys(?INDEX).

-spec keys(Value :: value()) -> [key()].
keys(Value) ->
    seppen_store:keys(?INDEX, Value).

%% priv

with_lock(Key, Fun) ->
    Lock = {{?MODULE, Key}, self()},
    case global:trans(Lock, Fun) of
        aborted ->
            {error, lock_failed};
        Result ->
            Result
    end.

set_value(Key, Value) ->
    OldHmac = hmac(Key),
    NewHmac = seppen_hash:hmac(Value),
    case store_value(NewHmac, Value) of
        ok ->
            Nodes = seppen_dispatch:all_shards(),
            case call_nodes(Nodes, ?INDEX, {set, Key, NewHmac}) of
                ok ->
                    maybe_delete_value(OldHmac, NewHmac),
                    ok;
                Error ->
                    rollback_index(Nodes, Key, OldHmac),
                    Error
            end;
        Error ->
            Error
    end.

store_value(Hmac, Value) ->
    call_nodes(seppen_dispatch:shards(Hmac), ?STORE, {set, Hmac, Value}).

delete_key(Key) ->
    case hmac(Key) of
        {ok, _} = OldHmac ->
            Nodes = seppen_dispatch:all_shards(),
            case call_nodes(Nodes, ?INDEX, {delete, Key}) of
                ok ->
                    maybe_delete_value(OldHmac, undefined),
                    ok;
                Error ->
                    rollback_index(Nodes, Key, OldHmac),
                    Error
            end;
        Error ->
            Error
    end.

rollback_index(Nodes, Key, {ok, Hmac}) ->
    call_nodes(Nodes, ?INDEX, {set, Key, Hmac});
rollback_index(Nodes, Key, {error, not_found}) ->
    call_nodes(Nodes, ?INDEX, {delete, Key}).

maybe_delete_value({ok, Hmac}, NewHmac) when Hmac =/= NewHmac ->
    case keys(Hmac) of
        [] ->
            call_nodes(seppen_dispatch:shards(Hmac), ?STORE, {delete, Hmac}),
            ok;
        _ ->
            ok
    end;
maybe_delete_value(_, _) ->
    ok.

get_value(Hmac) ->
    Nodes =
        lists:usort(
            seppen_dispatch:shards(Hmac)
        ),
    case Nodes of
        [] ->
            {error, no_shards};
        _ ->
            case rpc:multicall(Nodes, seppen_store, get, [?STORE, Hmac], 5000) of
                {Replies, []} ->
                    resolve_get(Replies);
                {_, BadNodes} ->
                    {error, {nodes_unavailable, BadNodes}}
            end
    end.

resolve_get(Replies) ->
    case lists:usort(Replies) of
        [{ok, _} = Reply] ->
            Reply;
        [{error, not_found}] ->
            {error, value_missing};
        [_] ->
            {error, replica_failure};
        _ ->
            {error, inconsistent_replicas}
    end.

call_nodes(Nodes0, Name, Request) ->
    Nodes = lists:usort(Nodes0),
    case Nodes of
        [] ->
            {error, no_shards};
        _ ->
            case gen_server:multi_call(Nodes, Name, Request, 5000) of
                {Replies, []} ->
                    check_replies(Nodes, Replies);
                {_, BadNodes} ->
                    {error, {nodes_unavailable, BadNodes}}
            end
    end.

check_replies(Nodes, Replies) when length(Nodes) =/= length(Replies) ->
    {error, incomplete_reply};
check_replies(_Nodes, Replies) ->
    case [Reply || {_Node, Reply} <- Replies, Reply =/= ok] of
        [] ->
            ok;
        Errors ->
            {error, {write_failed, Errors}}
    end.

%% application callbacks

start(_Type, _StartArgs) ->
    seppen:start_link().

stop(_State) ->
    ok.

%% supervisor callbacks

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% erlfmt:ignore
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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

resolve_get_test_() ->
    [
        ?_assertEqual({ok, <<"value">>}, resolve_get([{ok, <<"value">>}, {ok, <<"value">>}])),
        ?_assertEqual({error, value_missing}, resolve_get([{error, not_found}])),
        ?_assertEqual(
            {error, inconsistent_replicas},
            resolve_get([{ok, <<"one">>}, {ok, <<"two">>}])
        ),
        ?_assertEqual({error, no_shards}, call_nodes([], ?STORE, ignored)),
        ?_assertEqual(ok, check_replies([node@host], [{node@host, ok}])),
        ?_assertEqual(
            {error, {write_failed, [{error, failed}]}},
            check_replies([node@host], [{node@host, {error, failed}}])
        ),
        ?_assertEqual({error, incomplete_reply}, check_replies([node@host], []))
    ].

-endif.
