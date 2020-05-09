-module(seppen_store).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(kv, {uid, key, value}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    Tid = ets:new(?MODULE, [set, private, {keypos, #kv.uid}]),
    {ok, #{tid => Tid}}.

handle_call({set, Key, Value}, _, #{tid := Tid} = Ctx) ->
    true = ets:insert(Tid, #kv{uid = Key, key = Key, value = Value}),
    {reply, ok, Ctx};
handle_call({delete, Key}, _, #{tid := Tid} = Ctx) ->
    Head = #kv{key = '$1', _ = '_'},
    Match = {'==', '$1', Key},
    1 = ets:select_delete(Tid, [{Head, [Match], [true]}]),
    {reply, ok, Ctx};
handle_call({get, Key}, _, #{tid := Tid} = Ctx) ->
    Reply = case ets:lookup(Tid, Key) of
        [#kv{value = Value}] -> {ok, Value};
        [] -> {error, not_found}
    end,
    {reply, Reply, Ctx};
handle_call(list, _, #{tid := Tid} = Ctx) ->
    Head = #kv{key = '$1', _ = '_'},
    Keys = ets:select(Tid, [{Head, [], ['$1']}]),
    {reply, Keys, Ctx};
handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.
