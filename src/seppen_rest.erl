-module(seppen_rest).

-export([
    init/2,
    content_types_accepted/2,
    allowed_methods/2,
    content_types_provided/2
]).

-export([
    get_resource/2,
    set_resource/2,
    delete_resource/2
]).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, Ctx) ->
    Allowed = [<<"GET">>, <<"PUT">>, <<"DELETE">>],
    {Allowed, Req, Ctx}.

content_types_provided(Req, Ctx) ->
    Provided = [{<<"application/json">>, get_resource}],
    {Provided, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
    Accepted = [{<<"application/json">>, set_resource}],
    {Accepted, Req, Ctx}.


get_resource(Req, Ctx) ->
    Body = <<"[]">>,
    {Body, Req, Ctx}.

set_resource(#{path := <<"/">>} = Req0, Ctx) ->
    Req = cowboy_req:reply(405, Req0),
    {true, Req, Ctx};
set_resource(Req0, Ctx) ->
    Req = cowboy_req:reply(201, Req0),
    {true, Req, Ctx}.

delete_resource(#{path := <<"/">>} = Req0, Ctx) ->
    Req = cowboy_req:reply(405, Req0),
    {true, Req, Ctx};
delete_resource(Req, Ctx) ->
    {true, Req, Ctx}.
