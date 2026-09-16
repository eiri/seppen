-module(seppen_rest).

-include_lib("kernel/include/logger.hrl").

-define(META, #{domain => [seppen]}).

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
    logger:set_process_metadata(?META),
    ?LOG_INFO(#{status => up}),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[:key]", seppen_rest, []}
        ]}
    ]),
    Port = port(),
    TransportOpts = [{port, Port}],
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
    ?LOG_INFO(#{status => down}),
    {stop, {cowboy_down, Reason}, #{}}.

port() ->
    {ok, ConfigPort} = application:get_env(seppen, port),
    case os:getenv("SEPPEN_PORT") of
        false -> parse_port(ConfigPort);
        EnvPort -> parse_port(EnvPort)
    end.

parse_port(Port) when is_integer(Port), Port > 0, Port =< 65535 ->
    Port;
parse_port(Port) when is_list(Port) ->
    try
        parse_port(list_to_integer(Port))
    catch
        error:badarg -> error({invalid_port, Port})
    end;
parse_port(Port) ->
    error({invalid_port, Port}).

%% cowboy_rest callbacks

init(Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

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
    case seppen:hmac(Key) of
        {ok, ResourceHmac} -> {true, Req, Ctx#{hmac => ResourceHmac}};
        {error, not_found} -> {false, Req, Ctx}
    end.

generate_etag(#{path := <<"/">>} = Req, Ctx) ->
    {undefined, Req, Ctx};
generate_etag(Req, #{hmac := ResourceHmac} = Ctx) ->
    ETag = iolist_to_binary([$", seppen_hash:to_hex(ResourceHmac), $"]),
    {ETag, Req, Ctx}.

get_resource(#{method := Method, path := <<"/">>} = Req, Ctx) ->
    ?LOG_INFO(#{act => Method, path => <<"/">>}, ?META),
    Body = lists:join(<<"\n">>, seppen:keys()),
    {Body, Req, Ctx};
get_resource(#{method := Method, path := Path} = Req, Ctx) ->
    ?LOG_INFO(#{act => Method, path => Path}, ?META),
    Key = cowboy_req:binding(key, Req),
    case seppen:get(Key) of
        {ok, Value} ->
            case seppen_hash:hmac(Value) =:= maps:get(hmac, Ctx) of
                true -> {Value, Req, Ctx};
                false -> reply_error(503, Req, Ctx)
            end;
        {error, _} ->
            reply_error(503, Req, Ctx)
    end.

set_resource(#{method := Method, path := Path} = Req0, Ctx) ->
    ?LOG_INFO(#{act => Method, path => Path}, ?META),
    Key = cowboy_req:binding(key, Req0),
    case read_value(Req0) of
        {ok, Value, Req1} -> set_resource(Key, Value, Req1, Ctx);
        {error, too_large, Req1} -> reply_error(413, Req1, Ctx)
    end.

set_resource(Key, Value, Req, Ctx) ->
    Result =
        case {cowboy_req:header(<<"if-match">>, Req), maps:find(hmac, Ctx)} of
            {undefined, _} -> seppen:set(Key, Value);
            {_, {ok, ExpectedHmac}} -> seppen:maybe_set(Key, ExpectedHmac, Value);
            {_, error} -> {error, precondition_failed}
        end,
    case Result of
        ok -> {true, Req, Ctx};
        {error, precondition_failed} -> reply_error(412, Req, Ctx);
        {error, _} -> reply_error(503, Req, Ctx)
    end.

delete_resource(#{method := Method, path := Path} = Req, Ctx) ->
    ?LOG_INFO(#{act => Method, path => Path}, ?META),
    Key = cowboy_req:binding(key, Req),
    Result =
        case {cowboy_req:header(<<"if-match">>, Req), maps:find(hmac, Ctx)} of
            {undefined, _} -> seppen:delete(Key);
            {_, {ok, ExpectedHmac}} -> seppen:maybe_delete(Key, ExpectedHmac);
            {_, error} -> {error, precondition_failed}
        end,
    case Result of
        ok -> {true, Req, Ctx};
        {error, not_found} -> reply_error(404, Req, Ctx);
        {error, precondition_failed} -> reply_error(412, Req, Ctx);
        {error, _} -> reply_error(503, Req, Ctx)
    end.

read_value(Req) ->
    {ok, Limit} = application:get_env(seppen, max_value_size),
    read_value(Req, Limit, [], 0).

read_value(Req0, Limit, Chunks, Size) ->
    try cowboy_req:read_body(Req0, #{length => 65536}) of
        {Status, Data, Req1} ->
            NewSize = Size + byte_size(Data),
            case NewSize > Limit of
                true ->
                    {error, too_large, Req1};
                false when Status =:= more ->
                    read_value(Req1, Limit, [Data | Chunks], NewSize);
                false ->
                    {ok, iolist_to_binary(lists:reverse([Data | Chunks])), Req1}
            end
    catch
        exit:{request_error, payload_too_large, _} -> {error, too_large, Req0}
    end.

reply_error(Status, Req0, Ctx) ->
    Req = cowboy_req:reply(Status, #{}, <<>>, Req0),
    {stop, Req, Ctx}.
