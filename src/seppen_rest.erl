-module(seppen_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2,
    generate_etag/2,
    expires/2
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

generate_etag(#{path := <<"/_keys">>} = Req, Ctx) ->
    {undefined, Req, Ctx};
generate_etag(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    UID = seppen:uid(Key, [as_hex]),
    ETag = iolist_to_binary([$", UID, $"]),
    {ETag, Req, Ctx}.

expires(Req, Ctx) ->
    {undefined, Req, Ctx}.


get_resource(#{path := <<"/_keys">>} = Req, Ctx) ->
    Body = jiffy:encode(seppen:list()),
    {Body, Req, Ctx};
get_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    {ok, Value} = seppen:get(Key),
    {Value, Req, Ctx}.

set_resource(Req0, Ctx) ->
    Key = cowboy_req:binding(key, Req0),
    {ok, Value, Req1} = cowboy_req:read_body(Req0),
    Req = case seppen:set(Key, Value) of
        ok ->
            Req1;
        {error, conflict} ->
            %% FIXME! ok, the correct way to handle this is
            %% 1. get UID for payload and check if it's in storage in resource_exists
            %% 2. add method is_conflict, that always returns true
            cowboy_req:reply(409, Req1)
    end,
    {true, Req, Ctx}.

delete_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    ok = seppen:delete(Key),
    {true, Req, Ctx}.
