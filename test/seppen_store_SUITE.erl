-module(seppen_store_SUITE).

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
     storage_get/1,
     storage_get_missing/1,
     storage_list/1,
     storage_delete/1,
     storage_empty_list/1
]).


init_per_suite(Config) ->
     {ok, Apps} = application:ensure_all_started(seppen),
     [{apps, Apps} | Config].

end_per_suite(Config) ->
     Apps = ?config(apps, Config),
     [ok = application:stop(App) || App <- Apps],
     ok.

init_per_group(storage, Config) ->
     Config.

end_per_group(storage, Config) ->
     Config.

all() ->
     [{group, storage}].

groups() ->
     [{storage, [sequence], [
          storage_set,
          storage_get,
          storage_get_missing,
          storage_list,
          storage_delete,
          storage_empty_list
     ]}].


storage_set(_Config) ->
     lists:foreach(fun(I) ->
          Key = <<I:8>>,
          Value = <<I:32>>,
          ok = seppen:set(Key, Value)
     end, lists:seq(1, 10)).

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
