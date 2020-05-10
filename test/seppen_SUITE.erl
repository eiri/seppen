-module(seppen_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
     init_per_suite/1,
     end_per_suite/1,
     init_per_group/2,
     end_per_group/2,
     all/0,
     groups/0
]).

-export([
     storage_set/1,
     storage_set_conflict/1,
     storage_get/1,
     storage_get_missing/1,
     storage_member/1,
     storage_uid/1,
     storage_list/1,
     storage_delete/1,
     storage_empty_list/1
]).

-export([
     rest_put/1,
     rest_get/1,
     rest_get_if_none_match/1,
     rest_get_missing/1,
     rest_get_list/1,
     rest_delete/1,
     rest_delete_missing/1,
     rest_get_empty_list/1
]).


init_per_suite(Config) ->
     {ok, Apps} = application:ensure_all_started(seppen),
     [{apps, Apps} | Config].

end_per_suite(Config) ->
     Apps = ?config(apps, Config),
     [ok = application:stop(App) || App <- Apps],
     ok.

init_per_group(rest, Config) ->
     [{base_url, "http://localhost:21285"} | Config];
init_per_group(_Group, Config) ->
     Config.

end_per_group(rest, Config) ->
     {save_config, Config};
end_per_group(_Group, Config) ->
     Config.

all() ->
     [{group, storage}, {group, rest}].

groups() ->
     [{storage, [sequence], [
          storage_set,
          storage_set_conflict,
          storage_get,
          storage_get_missing,
          storage_member,
          storage_uid,
          storage_list,
          storage_delete,
          storage_empty_list
     ]},
     {rest, [sequence], [
          rest_put,
          rest_get,
          rest_get_if_none_match,
          rest_get_missing,
          rest_get_list,
          rest_delete,
          rest_delete_missing,
          rest_get_empty_list
     ]}].


storage_set(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          Value = <<I:32>>,
          ?assertEqual(ok, seppen:set(Key, Value))
     end, lists:seq(1, 10)).

storage_set_conflict(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          Value = <<(I-10):32>>,
          ?assertEqual({error, conflict}, seppen:set(Key, Value))
     end, lists:seq(11, 20)).

storage_get(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          {ok, Value} = seppen:get(Key),
          ?assertEqual(<<I:32>>, Value)
     end, lists:seq(1, 10)).

storage_get_missing(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertEqual({error, not_found}, seppen:get(Key))
     end, lists:seq(11, 20)).

storage_member(_Config) ->
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

storage_uid(_Config) ->
     %% presented
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assert(is_binary(seppen:uid(Key)))
     end, lists:seq(1, 10)),
     %% missing
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertMatch({error, _}, seppen:uid(Key))
     end, lists:seq(11, 20)).

storage_list(_Config) ->
     Expect = [<<I:8>> || I <- lists:seq(1, 10)],
     Keys = seppen:list(),
     ?assertEqual(Expect, Keys).

storage_delete(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          ?assertEqual(ok, seppen:delete(Key))
     end, lists:seq(1, 10)).

storage_empty_list(_Config) ->
     ?assertEqual([], seppen:list()).


rest_put(Config) ->
     BaseURL = ?config(base_url, Config),
     Payloads = lists:map(fun(I) ->
          URL = io_lib:format("~s/~b", [BaseURL, I]),
          Payload = jiffy:encode(#{<<"number">> => I}),
          Req = {URL, [], "application/json", Payload},
          {ok, Resp} = httpc:request(put, Req, [], []),
          {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
          ?assertEqual(201, Code),
          ?assertEqual([], Body),
          {I, Payload}
     end, lists:seq(11, 20)),
     NewConfig = [{payloads, Payloads}],
     {save_config, NewConfig}.

rest_get(Config) ->
     BaseURL = ?config(base_url, Config),
     {rest_put, OldConfig} = ?config(saved_config, Config),
     Payloads = ?config(payloads, OldConfig),
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

rest_get_list(Config) ->
     BaseURL = ?config(base_url, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     Expected = [integer_to_binary(I) || I <- lists:seq(11, 20)],
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     ?assertEqual(Expected, jiffy:decode(Body)).

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

rest_get_empty_list(Config) ->
     BaseURL = ?config(base_url, Config),
     URL = io_lib:format("~s/_keys", [BaseURL]),
     {ok, Resp} = httpc:request(URL),
     {{_HTTPVer, Code, _Reason}, _Headers, Body} = Resp,
     ?assertEqual(200, Code),
     ?assertEqual([], jiffy:decode(Body)).
