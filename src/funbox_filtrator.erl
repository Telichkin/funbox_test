-module(funbox_filtrator).
-behaviour(gen_server).
-import(funbox_utils, [init_periodic_message/1, continue_periodic_message/4]).

-export([start_link/2]).
-export([init/1, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DELAY_MS, 10).
-record(state, {t_ref, start_t, s_mod, primes}).

start_link(MaxN, StoreModule) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxN, StoreModule], []).

init([MaxN, StoreModule]) ->
  {TimerRef, StartTime} = init_periodic_message(filter),
  {ok, #state{t_ref = TimerRef,
              start_t = StartTime,
              s_mod = StoreModule,
              primes = primes(lists:seq(2, MaxN))}}.

handle_info(filter, #state{s_mod = Store} = S) ->
  TimerRef = continue_periodic_message(filter, ?DELAY_MS, S#state.start_t, S#state.t_ref),

  NumToFilter = Store:pop_all(),
  Store:save_primes([N || N <- NumToFilter, lists:member(N, S#state.primes)]),
  {noreply, S#state{t_ref = TimerRef}}.

primes([]) -> [];
primes([H | T]) -> [H] ++ primes([N || N <- T, N rem H /= 0]).
