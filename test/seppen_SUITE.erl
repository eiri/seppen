-module(seppen_SUITE).

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
     store_set/1,
     store_get/1,
     store_get_missing/1,
     store_member/1,
     store_hmac/1,
     store_keys/1,
     store_delete/1,
     store_empty_keys/1,
     store_keys_1/1
]).

-export([
     rest_put/1,
     rest_put_conflict/1,
     rest_get/1,
     rest_get_if_none_match/1,
     rest_put_if_match/1,
     rest_get_missing/1,
     rest_get_keys/1,
     rest_delete/1,
     rest_delete_missing/1,
     rest_get_empty_keys/1
]).


init_per_suite(Config) ->
     {ok, Apps} = application:ensure_all_started(seppen),
     [{apps, Apps} | Config].

end_per_suite(Config) ->
     Apps = ?config(apps, Config),
     [ok = application:stop(App) || App <- Apps],
     ok.

init_per_group(rest, Config) ->
     Payloads = lists:map(fun(I) ->
          {I, jiffy:encode(#{<<"number">> => I})}
     end, lists:seq(11, 20)),
     [{base_url, "http://localhost:21285"}, {payloads, Payloads} | Config];
init_per_group(_Group, Config) ->
     Config.

end_per_group(rest, Config) ->
     {save_config, Config};
end_per_group(_Group, _Config) ->
     ok.

init_per_testcase(store_keys_1, Config) ->
     Key1 = <<1:8>>,
     Key2 = <<2:8>>,
     Key3 = <<3:8>>,
     Value = crypto:strong_rand_bytes(32),
     Value2 = crypto:strong_rand_bytes(32),
     ok = seppen:set(Key1, Value),
     ok = seppen:set(Key2, Value2),
     ok = seppen:set(Key3, Value),
     {ok, VHmac1} = seppen:hmac(Key1),
     {ok, VHmac2} = seppen:hmac(Key2),
     {ok, VHmac3} = seppen:hmac(Key3),
     ?assertEqual(VHmac3, VHmac1),
     ?assertNotEqual(VHmac2, VHmac1),
     Keys = [Key1, Key2, Key3],
     [{vhmac, VHmac1}, {keys, Keys} | Config];
init_per_testcase(_, Config) ->
     Config.

end_per_testcase(store_keys_1, Config) ->
     Keys = ?config(keys, Config),
     [ok = seppen:delete(Key) || Key <- Keys];
end_per_testcase(_, _Config) ->
     ok.


all() ->
     [{group, store}, {group, rest}].

groups() ->
     [{store, [sequence], [
          store_set,
          store_get,
          store_get_missing,
          store_member,
          store_hmac,
          store_keys,
          store_delete,
          store_empty_keys,
          store_keys_1
     ]},
     {rest, [sequence], [
          rest_put,
          rest_put_conflict,
          rest_get,
          rest_get_if_none_match,
          rest_put_if_match,
          rest_get_missing,
          rest_get_keys,
          rest_delete,
          rest_delete_missing,
          rest_get_empty_keys
     ]}].


store_set(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          Value = <<I:32>>,
          ?assertEqual(ok, seppen:set(Key, Value))
     end, lists:seq(1, 10)).

store_get(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          {ok, Value} = seppen:get(Key),
          ?assertEqual(<<I:32>>, Value)
     end, lists:seq(1, 10)).

store_get_missing(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertEqual({error, not_found}, seppen:get(Key))
     end, lists:seq(11, 20)).

store_member(_Config) ->
     %% presented
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assert(seppen:member(Key))
     end, lists:seq(1, 10)),
     %% missing
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertNot(seppen:member(Key))
     end, lists:seq(11, 20)).

store_hmac(_Config) ->
     %% presented
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertMatch({ok, _}, seppen:hmac(Key))
     end, lists:seq(1, 10)),
     %% missing
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertMatch({error, _}, seppen:hmac(Key))
     end, lists:seq(11, 20)).

store_keys(_Config) ->
     Expected = [<<I:8>> || I <- lists:seq(1, 10)],
     MemberPred = fun(K) -> lists:member(K, Expected) end,
     Keys = seppen:keys(),
     ?assertEqual(10, length(Keys)),
     ?assert(lists:all(MemberPred, Keys)).

store_delete(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertEqual(ok, seppen:delete(Key))
     end, lists:seq(1, 10)).

store_empty_keys(_Config) ->
     ?assertEqual([], seppen:keys()).

store_keys_1(Config) ->
     VHmac = ?config(vhmac, Config),
     [Key1, _Key2, Key3] = ?config(keys, Config),
     %%
     Keys1 = seppen:keys(VHmac),
     ?assertEqual(2, length(Keys1)),
     ?assert(lists:member(Key1, Keys1)),
     ?assert(lists:member(Key3, Keys1)),
     %%
     ok = seppen:set(Key1, crypto:strong_rand_bytes(32)),
     Keys2 = seppen:keys(VHmac),
     ?assertEqual([Key3], Keys2),
     %%
     ok = seppen:set(Key3, crypto:strong_rand_bytes(32)),
     ?assertEqual([], seppen:keys(VHmac)).


rest_put(Config) ->
     BaseURL = ?config(base_url, Config),
     Payloads = ?config(payloads, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {I, Payload} = lists:keyfind(I, 1, Payloads),
          Req = {URL, [], "application/json", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(201, Code),
          ?assertEqual([], Body)
     end, lists:seq(11, 20)).

rest_put_conflict(Config) ->
     BaseURL = ?config(base_url, Config),
     Payloads = ?config(payloads, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {I, Payload} = lists:keyfind(I, 1, Payloads),
          ETag = [$", seppen_hash:to_hex(<<I:64>>), $"],
          Headers = [{"if-match", ETag}],
          Req = {URL, Headers, "application/json", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(412, Code),
          ?assertEqual([], Body)
     end, lists:seq(11, 20)).

rest_get(Config) ->
     BaseURL = ?config(base_url, Config),
     Payloads = ?config(payloads, Config),
     ETags = lists:map(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {I, Expected} = lists:keyfind(I, 1, Payloads),
          {ok, Resp} = httpc:request(URL),
          {{_HTTPVer, Code, _Reason}, Headers, Body} = Resp,
          ?assertEqual(200, Code),
          ?assertEqual(Expected, list_to_binary(Body)),
          ?assert(lists:keymember("etag", 1, Headers)),
          {"etag", ETag} = lists:keyfind("etag", 1, Headers),
          {I, ETag}
     end, lists:seq(11, 20)),
     NewConfig = [{etags, ETags}],
     {save_config, NewConfig}.

rest_get_if_none_match(Config) ->
     BaseURL = ?config(base_url, Config),
     {rest_get, OldConfig} = ?config(saved_config, Config),
     ETags = ?config(etags, OldConfig),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {I, ETag} = lists:keyfind(I, 1, ETags),
          Req = {URL, [{"if-none-match", ETag}]},
          {ok, Resp} = httpc:request(get, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(304, Code),
          ?assertEqual([], Body)
     end, lists:seq(11, 20)),
     NewConfig = [{etags, ETags}],
     {save_config, NewConfig}.

rest_put_if_match(Config) ->
     BaseURL = ?config(base_url, Config),
     {rest_get_if_none_match, OldConfig} = ?config(saved_config, Config),
     ETags = ?config(etags, OldConfig),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          Payload = jiffy:encode(#{<<"number">> => I+10}),
          {I, ETag} = lists:keyfind(I, 1, ETags),
          Headers = [{"if-match", ETag}],
          Req = {URL, Headers, "application/json", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(204, Code),
          ?assertEqual([], Body)
     end, lists:seq(11, 20)).

rest_get_missing(Config) ->
     BaseURL = ?config(base_url, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {ok, Resp} = httpc:request(URL),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(404, Code),
          ?assertEqual([], Body)
     end, lists:seq(21, 30)).

rest_get_keys(Config) ->
     BaseURL = ?config(base_url, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     Expected = [integer_to_binary(I) || I <- lists:seq(11, 20)],
     MemberPred = fun(K) -> lists:member(K, Expected) end,
     Keys = jiffy:decode(Body),
     ?assertEqual(10, length(Keys)),
     ?assert(lists:all(MemberPred, Keys)).

rest_delete(Config) ->
     BaseURL = ?config(base_url, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {ok, Resp} = httpc:request(delete, {URL, []}, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(204, Code),
          ?assertEqual([], Body)
     end, lists:seq(11, 20)).

rest_delete_missing(Config) ->
     BaseURL = ?config(base_url, Config),
     lists:foreach(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          {ok, Resp} = httpc:request(delete, {URL, []}, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(404, Code),
          ?assertEqual([], Body)
     end, lists:seq(21, 30)).

rest_get_empty_keys(Config) ->
     BaseURL = ?config(base_url, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     ?assertEqual([], jiffy:decode(Body)).
