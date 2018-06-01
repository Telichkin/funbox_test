-module(suite_acceptance).
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  [os:putenv(Key, Value) || {Key, Value} <- [
    {"F_REDIS_HOST", "localhost"}, {"F_REDIS_PORT", "6379"}, {"F_REDIS_DB", "0"},
    {"F_MAX_NUM", "100"}, {"F_QUEUE_KEY", "test-queue"}, {"F_RESULT_KEY", "test-res"}
  ]],
  {ok, _} = application:ensure_all_started(funbox).

teardown(_) ->
  funbox_store:pop_all(), funbox_store:flush_primes(),
  application:stop(funbox).

t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.

all() ->
  [
    t("Should generate all available primes for a second", fun(_) ->
      timer:sleep(1000),
      PrimesCountWhenMaxNumberIs100 = 25,
      ?_assertEqual(PrimesCountWhenMaxNumberIs100, length(funbox_store:get_primes()))
    end)
  ].