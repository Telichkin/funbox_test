-module(suite_generator).
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  MaxNumber = 10,
  {ok, StorePid} = funbox_fake_store:start_link(),
  {ok, GeneratorPid} = funbox_generator:start_link(MaxNumber, funbox_fake_store),
  [StorePid, GeneratorPid].

teardown([StorePid, GeneratorPid]) ->
  gen_server:stop(GeneratorPid), gen_server:stop(StorePid).

t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.

all() ->
  [
    t("Should generate numbers in range 2..MaxNumber", fun(_) ->
      timer:sleep(1000),
      Numbers = funbox_fake_store:get_numbers(),
      ?_assertEqual(ordsets:from_list(Numbers), ordsets:from_list(lists:seq(2, 10)))
    end),

    t("Should generate 3000 ± 30 numbers in a second", fun(_) ->
      funbox_fake_store:pop_all(),
      timer:sleep(1000),
      Numbers = funbox_fake_store:get_numbers(),
      ?_assert((2970 =< length(Numbers)) and (length(Numbers) =< 3030))
    end)
  ].
