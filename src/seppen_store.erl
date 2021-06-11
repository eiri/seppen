-module(seppen_store).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-define(META, #{domain => [seppen], name => store}).

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
    ?LOG_INFO(#{act => get, key => binary:encode_hex(Key)}, ?META),
    case ets:lookup(Table, Key) of
        [#kv{value = Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

member(Table, Key) ->
    ?LOG_INFO(#{act => member, key => binary:encode_hex(Key)}, ?META),
    ets:member(Table, Key).

keys(Table) ->
    ?LOG_INFO(#{act => keys}, ?META),
    Head = #kv{key = '$1', _ = '_'},
    ets:select(Table, [{Head, [], ['$1']}]).

keys(Table, Value) ->
    ?LOG_INFO(#{act => keys}, ?META),
    Head = #kv{key = '$1', value = '$2'},
    Guards = [{'==', '$2', Value}],
    ets:select(Table, [{Head, Guards, ['$1']}]).

init([Name]) ->
    logger:set_process_metadata(#{domain => [seppen], name => Name}),
    ?LOG_INFO(#{status => up}),
    Tid = ets:new(Name, [
        set,
        named_table,
        protected,
        {read_concurrency, true},
        {keypos, #kv.key}
    ]),
    {ok, #{tid => Tid}}.

handle_call({set, Key, Value}, _, #{tid := Tid} = Ctx) ->
    ?LOG_INFO(#{act => set, cb => handle_call, key => binary:encode_hex(Key)}),
    true = ets:insert(Tid, #kv{key = Key, value = Value}),
    {reply, ok, Ctx};
handle_call({delete, Key}, _, #{tid := Tid} = Ctx) ->
    ?LOG_INFO(#{act => delete, cb => handle_call, key => binary:encode_hex(Key)}),
    true = ets:delete(Tid, Key),
    {reply, ok, Ctx};
handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast({set, Key, Value}, #{tid := Tid} = Ctx) ->
    ?LOG_INFO(#{act => set, cb => handle_cast, key => binary:encode_hex(Key)}),
    true = ets:insert(Tid, #kv{key = Key, value = Value}),
    {noreply, Ctx};
handle_cast({delete, Key}, #{tid := Tid} = Ctx) ->
    ?LOG_INFO(#{act => delete, cb => handle_cast, key => binary:encode_hex(Key)}),
    true = ets:delete(Tid, Key),
    {noreply, Ctx};
handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.
