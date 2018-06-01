-module(suite_store).
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  RedisHost = "localhost", RedisPort = 6379, RedisDB = 0,
  QueueKey = "funbox-queue", ResultSetKey = "primes",
  {ok, Pid} = funbox_store:start_link(RedisHost, RedisPort, RedisDB, QueueKey, ResultSetKey),
  Pid.

teardown(Pid) ->
  funbox_store:pop_all(), funbox_store:flush_primes(),
  gen_server:stop(Pid).

t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.

all() ->
  [
    t("Should save pushed numbers", fun(_) ->
      funbox_store:push_numbers([1, 2, 3]),
      ?_assertEqual([1, 2, 3], funbox_store:get_numbers())
    end),

    t("Should pop all pushed numbers", fun(_) ->
      funbox_store:push_numbers([9, 8, 7]),
      funbox_store:push_numbers([6, 5, 4]),
      [
        ?_assertEqual([9, 8, 7, 6, 5, 4], funbox_store:pop_all()),
        ?_assertEqual([], funbox_store:get_numbers())
      ]
    end),

    t("Should save primes without repetition", fun(_) ->
      funbox_store:save_primes([2, 3, 5]),
      funbox_store:save_primes([2]),
      ?_assertEqual([2, 3, 5], funbox_store:get_primes())
    end),

    t("Should delete all saved primes", fun(_) ->
      funbox_store:save_primes([7, 9]),
      funbox_store:flush_primes(),
      ?_assertEqual([], funbox_store:get_primes())
    end)
  ].