-module(seppen_dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
     init_per_suite/1,
     end_per_suite/1,
     all/0
]).

-export([
     test_put/1,
     test_get/1,
     test_get_if_none_match/1,
     test_get_list/1,
     test_delete/1,
     test_get_empty_list/1
]).


init_per_suite(Config) ->
     LocalHost = list_to_atom(net_adm:localhost()),
     Nodes = ['ct1-0-127@127.0.0.1', 'ct2-128-255@127.0.0.1'],
     lists:foreach(fun({Node, Port}) ->
          ct_slave:start(LocalHost, Node, [
               {erl_flags, "-pa ../../lib/*/ebin"},
               {env, [{"SEPPEN_PORT", Port}]}
          ])
     end, lists:zip(Nodes, ["21285", "21286"])),

     {_, []} = rpc:multicall(Nodes, application, ensure_all_started, [seppen]),

     URLs = ["http://localhost:21285", "http://localhost:21286"],
     [{nodes, Nodes}, {urls, URLs} | Config].

end_per_suite(Config) ->
     Nodes = ?config(nodes, Config),
     [ct_slave:stop(Node) || Node <- Nodes],
     ok.

all() ->
     [
          test_put,
          test_get,
          test_get_if_none_match,
          test_get_list,
          test_delete,
          test_get_empty_list
     ].


test_put(Config) ->
     Nodes = ?config(nodes, Config),
     [BaseURL, _] = ?config(urls, Config),
     Payloads = lists:map(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          Payload = jiffy:encode(#{<<"number">> => I}),
          Req = {URL, [], "application/json", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(201, Code),
          ?assertEqual([], Body),
          %% ask nodes where they think it should be
          Key = integer_to_binary(I),
          {Maps, []} = rpc:multicall(Nodes, seppen_dispatch, shards, [Key]),
          [Map1, Map2] = Maps,
          ?assertEqual(Map1, Map2),
          %% confirm that keys are indeed there
          [StoreNode] = Map1,
          [MissNode] = Nodes -- Map1,
          Cmd = [seppen_store, {member, Key}],
          OnNode = rpc:call(StoreNode, gen_server, call, Cmd),
          NotOnNode = rpc:call(MissNode, gen_server, call, Cmd),
          ?assert(OnNode),
          ?assertNot(NotOnNode),
          {I, Payload}
     end, lists:seq(1, 10)),
     NewConfig = [{payloads, Payloads}],
     {save_config, NewConfig}.

test_get(Config) ->
     URLs = ?config(urls, Config),
     {test_put, OldConfig} = ?config(saved_config, Config),
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

test_get_if_none_match(Config) ->
     URLs = ?config(urls, Config),
     {test_get, OldConfig} = ?config(saved_config, Config),
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

test_get_list(Config) ->
     [BaseURL, _] = ?config(urls, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     Expected = [integer_to_binary(I) || I <- lists:seq(1, 10)],
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     Expected = [integer_to_binary(I) || I <- lists:seq(1, 10)],
     MemberPred = fun(K) -> lists:member(K, Expected) end,
     Keys = jiffy:decode(Body),
     ?assertEqual(10, length(Keys)),
     ?assert(lists:all(MemberPred, Keys)).

test_delete(Config) ->
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
          Cmd = [seppen_store, {member, Key}],
          Reply = rpc:multicall(Nodes, gen_server, call, Cmd),
          ?assertEqual({[false, false], []}, Reply)
     end, lists:seq(1, 10)).

test_get_empty_list(Config) ->
     [BaseURL, _] = ?config(urls, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     ?assertEqual([], jiffy:decode(Body)).
