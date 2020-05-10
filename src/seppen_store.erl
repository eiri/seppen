-module(seppen_store).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(kv, {key, value}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    MasterKey = erlang:atom_to_binary(erlang:get_cookie(), unicode),
    Tid = ets:new(?MODULE, [set, private, {keypos, #kv.key}]),
    Idx = ets:new(?MODULE, [set, private, {keypos, #kv.key}]),
    {ok, #{tid => Tid, idx => Idx, mkey => MasterKey}}.

handle_call({set, Key, Value}, _, Ctx) ->
    #{
        tid := Tid,
        idx := Idx,
        mkey := MKey
    } = Ctx,
    UID = crypto:mac(hmac, sha256, MKey, Value),
    Reply =  case ets:member(Tid, UID) of
        true ->
            {error, conflict};
        false ->
            %% FIXME! actuall delete OldUID first
            true = ets:insert(Idx, #kv{key = Key, value = UID}),
            true = ets:insert(Tid, #kv{key = UID, value = Value}),
            ok
    end,
    {reply, Reply, Ctx};
handle_call({delete, Key}, _, #{tid := Tid, idx := Idx} = Ctx) ->
    [#kv{value = UID}] = ets:lookup(Idx, Key),
    true = ets:delete(Idx, Key),
    true = ets:delete(Tid, UID),
    {reply, ok, Ctx};
handle_call({member, Key}, _, #{idx := Idx} = Ctx) ->
    IsMemeber = ets:member(Idx, Key),
    {reply, IsMemeber, Ctx};
handle_call({uid, Key, Opts}, _, #{idx := Idx} = Ctx) ->
    Reply = case ets:lookup(Idx, Key) of
        [#kv{value = UID}] ->
            case lists:member(as_hex, Opts) of
                true -> to_hex(UID);
                false -> UID
            end;
        [] ->
            {error, not_found}
    end,
    {reply, Reply, Ctx};
handle_call({get, Key}, _, #{tid := Tid, idx := Idx} = Ctx) ->
    Reply = case ets:lookup(Idx, Key) of
        [#kv{value = UID}] ->
            [#kv{value = Value}] = ets:lookup(Tid, UID),
            {ok, Value};
        [] ->
            {error, not_found}
    end,
    {reply, Reply, Ctx};
handle_call(list, _, #{idx := Idx} = Ctx) ->
    Head = #kv{key = '$1', _ = '_'},
    Keys = ets:select(Idx, [{Head, [], ['$1']}]),
    {reply, Keys, Ctx};
handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.


to_hex(Bin) ->
    [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].
