-module(seppen_health).

-export([init/2]).

init(Req0, live) ->
    reply(200, <<"ok\n">>, Req0, live);
init(Req0, ready) ->
    case seppen_dispatch:ready() of
        true -> reply(200, <<"ok\n">>, Req0, ready);
        false -> reply(503, <<"not ready\n">>, Req0, ready)
    end.

reply(Status, Body, Req0, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Req = cowboy_req:reply(Status, Headers, Body, Req0),
    {ok, Req, State}.
