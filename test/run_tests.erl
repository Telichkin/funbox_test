-module(run_tests).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {inorder, lists:flatten([
    suite_generator:all(),
    suite_filtrator:all(),
    suite_store:all(),
    suite_acceptance:all()
  ])}.
