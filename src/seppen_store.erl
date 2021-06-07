-module(seppen_store).

-behaviour(gen_server).

-export([
    start_link/1,
    get/2,
    member/2,
    keys/1,
    keys/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(kv, {key, value}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [#kv{value = Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

member(Table, Key) ->
    ets:member(Table, Key).

keys(Table) ->
    Head = #kv{key = '$1', _ = '_'},
    ets:select(Table, [{Head, [], ['$1']}]).

keys(Table, Value) ->
    Head = #kv{key = '$1', value = '$2'},
    Guards = [{'==', '$2', Value}],
    ets:select(Table, [{Head, Guards, ['$1']}]).

init([Name]) ->
    Tid = ets:new(Name, [
        set,
        named_table,
        protected,
        {read_concurrency, true},
        {keypos, #kv.key}
    ]),
    {ok, #{tid => Tid}}.

handle_call({set, Key, Value}, _, #{tid := Tid} = Ctx) ->
    true = ets:insert(Tid, #kv{key = Key, value = Value}),
    {reply, ok, Ctx};
handle_call({delete, Key}, _, #{tid := Tid} = Ctx) ->
    true = ets:delete(Tid, Key),
    {reply, ok, Ctx};
handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast({set, Key, Value}, #{tid := Tid} = Ctx) ->
    true = ets:insert(Tid, #kv{key = Key, value = Value}),
    {noreply, Ctx};
handle_cast({delete, Key}, #{tid := Tid} = Ctx) ->
    true = ets:delete(Tid, Key),
    {noreply, Ctx};
handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.
