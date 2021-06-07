-module(seppen_dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
     init_per_suite/1,
     end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
     init_per_testcase/2,
     end_per_testcase/2,
     all/0,
     groups/0
]).

-export([
     dispatch_all_nodes/1,
     dispatch_nodedown/1,
     dispatch_nodeup/1,
     sharding_put/1,
     sharding_get/1,
     sharding_get_if_none_match/1,
     sharding_get_keys/1,
     sharding_delete/1,
     sharding_get_empty_keys/1
]).


init_per_suite(Config) ->
     LocalHost = list_to_atom(net_adm:localhost()),
     Nodes = ['ct1-0-127@127.0.0.1', 'ct2-128-255@127.0.0.1'],
     lists:foreach(fun({Node, Port}) ->
          ct_slave:start(LocalHost, Node, [
               {erl_flags, "-pa ../../lib/*/ebin"},
               {env, [{"SEPPEN_PORT", Port}]}
          ])
     end, lists:zip(Nodes, ["21286", "21287"])),

     {_, []} = rpc:multicall(Nodes, application, ensure_all_started, [seppen]),

     URLs = ["http://localhost:21286", "http://localhost:21287"],
     [{nodes, Nodes}, {urls, URLs} | Config].

end_per_suite(Config) ->
     Nodes = ?config(nodes, Config),
     [ct_slave:stop(Node) || Node <- Nodes],
     ok.

init_per_group(dispatch, Config) ->
     {ok, Apps} = application:ensure_all_started(seppen),
     [{apps, Apps} | Config];
init_per_group(_, Config) ->
     Config.

end_per_group(dispatch, Config) ->
     Apps = ?config(apps, Config),
     [ok = application:stop(App) || App <- Apps];
end_per_group(_, _Config) ->
     ok.

init_per_testcase(dispatch_nodedown, Config) ->
     [_, Node2] = ?config(nodes, Config),
     Result = ct_slave:stop(Node2),
     ?assertEqual({ok, Node2}, Result),
     Config;
init_per_testcase(_, Config) ->
     Config.

end_per_testcase(dispatch_nodedown, Config) ->
     [_, Node2] = ?config(nodes, Config),
     LocalHost = list_to_atom(net_adm:localhost()),
     Result = ct_slave:start(LocalHost, Node2, [
          {erl_flags, "-pa ../../lib/*/ebin"},
          {env, [{"SEPPEN_PORT", "21287"}]}
     ]),
     ?assertEqual({ok, Node2}, Result),
     Result2 = rpc:call(Node2, application, ensure_all_started, [seppen]),
     ?assertMatch({ok, _}, Result2),
     ok;
end_per_testcase(_, _Config) ->
     ok.


all() ->
     [{group, dispatch}, {group, sharding}].

groups() ->
     [{dispatch, [sequence], [
          dispatch_all_nodes,
          dispatch_nodedown,
          dispatch_nodeup
     ]},
     {sharding, [sequence], [
          sharding_put,
          sharding_get,
          sharding_get_if_none_match,
          sharding_get_keys,
          sharding_delete,
          sharding_get_empty_keys
     ]}].


dispatch_all_nodes(Config) ->
     [Node1, Node2] = Nodes = ?config(nodes, Config),
     ?assertEqual([Node1], seppen_dispatch:shards(<<64:8, 0, 0, 0>>)),
     ?assertEqual([Node2], seppen_dispatch:shards(<<192:8, 0, 0, 0>>)),
     Shards = seppen_dispatch:all_shards(),
     ?assertEqual(Nodes, lists:sort(Shards)).

dispatch_nodedown(Config) ->
     [Node1, _] = ?config(nodes, Config),
     ?assertEqual([Node1], seppen_dispatch:shards(<<64:8, 0, 0, 0>>)),
     %% because I don't have hand-off yet
     ?assertEqual([], seppen_dispatch:shards(<<192:8, 0, 0, 0>>)),
     ?assertEqual([Node1], seppen_dispatch:all_shards()).

dispatch_nodeup(Config) ->
     dispatch_all_nodes(Config).


sharding_put(Config) ->
     Nodes = ?config(nodes, Config),
     [BaseURL, _] = ?config(urls, Config),
     Payloads = lists:map(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          Payload = <<"number => ", I>>,
          Req = {URL, [], "application/octet-stream", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(201, Code),
          ?assertEqual([], Body),
          %% ask nodes where they think it should be
          VHmac = seppen_hash:hmac(Payload),
          {Maps, []} = rpc:multicall(Nodes, seppen_dispatch, shards, [VHmac]),
          [Map1, Map2] = Maps,
          ?assertEqual(Map1, Map2),
          %% confirm that keys are indeed there
          [StoreNode] = Map1,
          [MissNode] = Nodes -- Map1,
          Args = [seppen_store, VHmac],
          OnNode = rpc:call(StoreNode, seppen_store, member, Args),
          NotOnNode = rpc:call(MissNode, seppen_store, member, Args),
          ?assert(OnNode),
          ?assertNot(NotOnNode),
          {I, Payload}
     end, lists:seq(1, 10)),
     NewConfig = [{payloads, Payloads}],
     {save_config, NewConfig}.

sharding_get(Config) ->
     URLs = ?config(urls, Config),
     {sharding_put, OldConfig} = ?config(saved_config, Config),
     Payloads = ?config(payloads, OldConfig),
     ETags = lists:map(fun(I) ->
          [ETag1, ETag2] = lists:foldl(fun(URL0, Acc) ->
               URL = io_lib:format("~s/~b", [URL0, I]),
               {I, Expected} = lists:keyfind(I, 1, Payloads),
               {ok, Resp} = httpc:request(URL),
               {{_HTTPVer, Code, _Reason}, Headers, Body} = Resp,
               ?assertEqual(200, Code),
               ?assertEqual(Expected, list_to_binary(Body)),
               ?assert(lists:keymember("etag", 1, Headers)),
               {"etag", ETag} = lists:keyfind("etag", 1, Headers),
               [ETag | Acc]
          end, [], URLs),
          ?assertEqual(ETag2, ETag1),
          {I, ETag1}
     end, lists:seq(1, 10)),
     NewConfig = [{etags, ETags}],
     {save_config, NewConfig}.

sharding_get_if_none_match(Config) ->
     URLs = ?config(urls, Config),
     {sharding_get, OldConfig} = ?config(saved_config, Config),
     ETags = ?config(etags, OldConfig),
     lists:foreach(fun(I) ->
          lists:foreach(fun(URL0) ->
               URL = io_lib:format("~s/~b", [URL0, I]),
               {I, ETag} = lists:keyfind(I, 1, ETags),
               Req = {URL, [{"if-none-match", ETag}]},
               {ok, Resp} = httpc:request(get, Req, [], []),
               {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
               ?assertEqual(304, Code),
               ?assertEqual([], Body)
          end, URLs)
     end, lists:seq(1, 10)).

sharding_get_keys(Config) ->
     [BaseURL, _] = ?config(urls, Config),
     URL = io_lib:format("~s/", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     Expected = [integer_to_list(I) || I <- lists:seq(1, 10)],
     MemberPred = fun(K) -> lists:member(K, Expected) end,
     Keys = string:split(Body, "\n", all),
     ?assertEqual(10, length(Keys)),
     ?assert(lists:all(MemberPred, Keys)).

sharding_delete(Config) ->
     Nodes = ?config(nodes, Config),
     [BaseURL, _] = ?config(urls, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {ok, Resp} = httpc:request(delete, {URL, []}, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(204, Code),
          ?assertEqual([], Body),
          %% confirm that key gone on both nodes
          Key = integer_to_binary(I),
          Args = [seppen_store, Key],
          Reply = rpc:multicall(Nodes, seppen_store, member, Args),
          ?assertEqual({[false, false], []}, Reply)
     end, lists:seq(1, 10)).

sharding_get_empty_keys(Config) ->
     [BaseURL, _] = ?config(urls, Config),
     URL = io_lib:format("~s/", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     ?assertEqual("", Body).
