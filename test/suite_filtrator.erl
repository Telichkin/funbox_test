-module(suite_filtrator).
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  MaxNumber = 28,
  {ok, StorePid} = funbox_fake_store:start_link(),
  {ok, FiltratorPid} = funbox_filtrator:start_link(MaxNumber, funbox_fake_store),
  [StorePid, FiltratorPid].

teardown([StorePid, FiltratorPid]) ->
  gen_server:stop(FiltratorPid), gen_server:stop(StorePid).

t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.

all() ->
  [
    t("Should pop all numbers every 10 ms", fun(_) ->
      funbox_fake_store:push_numbers(lists:seq(2, 10)),
      timer:sleep(10),
      N1 = funbox_fake_store:get_numbers(),

      funbox_fake_store:push_numbers(lists:seq(2, 10)),
      timer:sleep(10),
      N2 = funbox_fake_store:get_numbers(),

      [?_assertEqual([], N1), ?_assertEqual([], N2)]
    end),

    t("Should save all available primes", fun(_) ->
      funbox_fake_store:push_numbers(lists:seq(2, 28)),
      timer:sleep(10),
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23], funbox_fake_store:get_primes())
    end),

    t("Should save primes incrementally", fun(_) ->
      funbox_fake_store:push_numbers([2, 4, 8, 10]),
      timer:sleep(10),
      P1 = funbox_fake_store:get_primes(),

      funbox_fake_store:push_numbers([3, 5, 6, 7, 9]),
      timer:sleep(10),
      P2 = funbox_fake_store:get_primes(),

      [?_assertEqual([2], P1), ?_assertEqual([2, 3, 5, 7], P2)]
    end),

    t("Should not save primes which greater than MaxNumber", fun(_) ->
      funbox_fake_store:push_numbers([2, 29]),
      timer:sleep(10),
      ?_assertEqual([2], funbox_fake_store:get_primes())
    end)
  ].
