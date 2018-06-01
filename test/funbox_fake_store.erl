-module(funbox_fake_store).
-behaviour(gen_server).

-export([start_link/0, push_numbers/1, get_numbers/0, pop_all/0, save_primes/1, get_primes/0]).
-export([init/1, handle_cast/2, handle_call/3]).

-define(SERVER, ?MODULE).


start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, ok, []).

push_numbers(Numbers) when is_list(Numbers) ->
  gen_server:cast(?SERVER, {push_numbers, Numbers}).

get_numbers() ->
  gen_server:call(?SERVER, get_numbers).

pop_all() ->
  gen_server:call(?SERVER, pop_all).

save_primes(Primes) when is_list(Primes) ->
  gen_server:cast(?SERVER, {save_primes, Primes}).

get_primes() ->
  gen_server:call(?SERVER, get_primes).

init(ok) ->
  {ok, {[], []}}.

handle_cast({push_numbers, Numbers}, {CurrentNumbers, P}) ->
  {noreply, {Numbers ++ CurrentNumbers, P}};
handle_cast({save_primes, Primes}, {N, CurrentPrimes}) ->
  {noreply, {N, Primes ++ CurrentPrimes}}.

handle_call(get_numbers, _From, {CurrentNumbers, _} = S) ->
  {reply, CurrentNumbers, S};
handle_call(pop_all, _From, {CurrentNumbers, P}) ->
  {reply, CurrentNumbers, {[], P}};
handle_call(get_primes, _From, {_, Primes} = S) ->
  {reply, ordsets:to_list(ordsets:from_list(Primes)), S}.
