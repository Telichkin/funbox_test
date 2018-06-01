-module(funbox_generator).
-behaviour(gen_server).
-import(funbox_utils, [init_periodic_message/1, continue_periodic_message/4]).

-export([start_link/2]).
-export([init/1, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DELAY_MS, 10).
-define(NUM_IN_MS, 3).
-record(state, {t_ref, start_t, s_mod, max_n}).

start_link(MaxN, StoreModule) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxN, StoreModule], []).

init([MaxN, StoreModule]) ->
  {TimerRef, StartTime} = init_periodic_message(generate),
  rand:seed(exs1024s),
  {ok, #state{t_ref = TimerRef,
              start_t = StartTime,
              s_mod = StoreModule,
              max_n = MaxN}}.

handle_info(generate, #state{} = S) ->
  TimerRef = continue_periodic_message(generate, ?DELAY_MS, S#state.start_t, S#state.t_ref),

  RandomNumbers = [rand:uniform(S#state.max_n - 1) + 1 || _ <- lists:seq(1, ?NUM_IN_MS * ?DELAY_MS)],
  apply(S#state.s_mod, push_numbers, [RandomNumbers]),
  {noreply, S#state{t_ref = TimerRef}}.
