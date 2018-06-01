-module(funbox_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  {ok, { {one_for_all, 4, 1}, children()} }.

children() ->
  MaxNumber = list_to_integer(os:getenv("F_MAX_NUM")),
  [
    worker(funbox_store, [os:getenv("F_REDIS_HOST"), list_to_integer(os:getenv("F_REDIS_PORT")),
                          list_to_integer(os:getenv("F_REDIS_DB")), os:getenv("F_QUEUE_KEY"),
                          os:getenv("F_RESULT_KEY")]),
    worker(funbox_filtrator, [MaxNumber, funbox_store]),
    worker(funbox_generator, [MaxNumber, funbox_store])
  ].

worker(Module, ArgList) ->
  {Module, {Module, start_link, ArgList}, permanent, 1000, worker, [Module]}.
