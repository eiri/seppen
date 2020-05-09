-module(seppen_store).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-record(kv, {uid, key, val}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    Tid = ets:new(?MODULE, [set, private, {keypos, #kv.uid}]),
    {ok, #{tid => Tid}}.

handle_call(_, _, Ctx) ->
    {stop, unknown_call, Ctx}.

handle_cast(_, Ctx) ->
    {stop, unknown_cast, Ctx}.
