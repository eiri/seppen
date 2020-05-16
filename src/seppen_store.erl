-module(seppen_store).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(kv, {key, value}).


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


init([]) ->
    Tid = ets:new(?MODULE, [set, private, {keypos, #kv.key}]),
    {ok, #{tid => Tid}}.

handle_call({get, Key}, _, #{tid := Tid} = Ctx) ->
    case ets:lookup(Tid, Key) of
        [#kv{value = Value}] ->
            {reply, {ok, Value}, Ctx};
        [] ->
            {reply, {error, not_found}, Ctx}
    end;
handle_call({member, Key}, _, #{tid := Tid} = Ctx) ->
    IsMemeber = ets:member(Tid, Key),
    {reply, IsMemeber, Ctx};
handle_call(list, _, #{tid := Tid} = Ctx) ->
    Head = #kv{key = '$1', _ = '_'},
    Keys = ets:select(Tid, [{Head, [], ['$1']}]),
    {reply, Keys, Ctx};
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
