-module(seppen_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2
]).

-export([
    get_resource/2,
    set_resource/2,
    delete_resource/2
]).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(#{path := <<"/_keys">>} = Req, Ctx) ->
    {[<<"GET">>], Req, Ctx};
allowed_methods(Req, Ctx) ->
    Allowed = [<<"GET">>, <<"PUT">>, <<"DELETE">>],
    {Allowed, Req, Ctx}.

content_types_provided(Req, Ctx) ->
    Provided = [{<<"application/json">>, get_resource}],
    {Provided, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
    Accepted = [{<<"application/json">>, set_resource}],
    {Accepted, Req, Ctx}.

resource_exists(#{path := <<"/_keys">>} = Req, Ctx) ->
    {true, Req, Ctx};
resource_exists(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    IsMember = seppen:member(Key),
    {IsMember, Req, Ctx}.


get_resource(#{path := <<"/_keys">>} = Req, Ctx) ->
    Body = jiffy:encode(seppen:list()),
    {Body, Req, Ctx};
get_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    {ok, Value} = seppen:get(Key),
    {Value, Req, Ctx}.

set_resource(Req0, Ctx) ->
    Key = cowboy_req:binding(key, Req0),
    {ok, Value, Req} = cowboy_req:read_body(Req0),
    ok = seppen:set(Key, Value),
    {true, Req, Ctx}.

delete_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    ok = seppen:delete(Key),
    {true, Req, Ctx}.
