-module(seppen_rest).

%% gen_server api & callbacks.
%% cheating here, using gen_server funcs without declaring behaviour
-export([
    start_link/0,
    init/1,
    terminate/2,
    handle_info/2
]).

%% cowboy_rest callback
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    resource_exists/2,
    generate_etag/2
]).

-export([
    get_resource/2,
    set_resource/2,
    delete_resource/2
]).


%% gen_server callbacks

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", seppen_rest, []},
            {"/:key", seppen_rest, []}
        ]}
    ]),
    Port = os:getenv("SEPPEN_PORT", "21285"),
    TransportOpts = [{port, list_to_integer(Port)}],
    ProtocolOpts = #{
        env => #{dispatch => Dispatch}
    },
    {ok, Pid} = cowboy:start_clear(http, TransportOpts, ProtocolOpts),
    process_flag(trap_exit, true),
    monitor(process, Pid),
    {ok, #{pid => Pid}}.

terminate(_Reason, _Ctx) ->
    cowboy:stop_listener(http),
    ok.

handle_info({'DOWN', _, process, Pid, Reason}, #{pid := Pid}) ->
    {stop, {cowboy_down, Reason}, #{}}.


%% cowboy_rest callbacks

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(#{path := <<"/">>} = Req, Ctx) ->
    {[<<"GET">>], Req, Ctx};
allowed_methods(Req, Ctx) ->
    Allowed = [<<"GET">>, <<"PUT">>, <<"DELETE">>],
    {Allowed, Req, Ctx}.

content_types_provided(Req, Ctx) ->
    Provided = [{<<"application/octet-stream">>, get_resource}],
    {Provided, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
    Accepted = [{<<"application/octet-stream">>, set_resource}],
    {Accepted, Req, Ctx}.

resource_exists(#{path := <<"/">>} = Req, Ctx) ->
    {true, Req, Ctx};
resource_exists(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    IsMember = seppen:member(Key),
    {IsMember, Req, Ctx}.

generate_etag(#{path := <<"/">>} = Req, Ctx) ->
    {undefined, Req, Ctx};
generate_etag(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    {ok, Hmac} = seppen:hmac(Key),
    ETag = iolist_to_binary([$", seppen_hash:to_hex(Hmac), $"]),
    {ETag, Req, Ctx}.

get_resource(#{path := <<"/">>} = Req, Ctx) ->
    Body = lists:join(<<"\n">>, seppen:keys()),
    {Body, Req, Ctx};
get_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    {ok, Value} = seppen:get(Key),
    {Value, Req, Ctx}.

set_resource(Req0, Ctx) ->
    Key = cowboy_req:binding(key, Req0),
    {ok, Value, Req1} = cowboy_req:read_body(Req0),
    ok = seppen:set(Key, Value),
    {true, Req1, Ctx}.

delete_resource(Req, Ctx) ->
    Key = cowboy_req:binding(key, Req),
    ok = seppen:delete(Key),
    {true, Req, Ctx}.
