-module(funbox_store).
-behaviour(gen_server).

-export([start_link/5, push_numbers/1, get_numbers/0, pop_all/0, save_primes/1, get_primes/0, flush_primes/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-define(SERVER, ?MODULE).

-record(state, {redis, queue, primes}).

start_link(RedisHost, RedisPort, RedisDB, QueueKey, ResultSetKey) ->
  Arg = {RedisHost, RedisPort, RedisDB, QueueKey, ResultSetKey},
  gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).

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

flush_primes() ->
  gen_server:call(?SERVER, flush_primes).

init({Host, Port, DB, QueueKey, ResultSetKey}) ->
  self() ! {redis_conn, {Host, Port, DB}},
  {ok, #state{queue = QueueKey, primes = ResultSetKey}}.

handle_info({redis_conn, {Host, Port, DB}}, #state{} = S) ->
  {ok, Pid} = eredis:start_link(Host, Port, DB),
  {noreply, S#state{redis = Pid}}.

handle_cast({push_numbers, Numbers}, #state{} = S) ->
  eredis:q(S#state.redis, ["LPUSH", S#state.queue] ++ [integer_to_list(N) || N <- Numbers]),
  {noreply, S};
handle_cast({save_primes, Primes}, #state{} = S) ->
  eredis:q(S#state.redis, ["SADD", S#state.primes] ++ [integer_to_list(N) || N <- Primes]),
  {noreply, S}.

handle_call(get_numbers, _From, #state{} = S) ->
  {ok, Numbers} = eredis:q(S#state.redis, ["LRANGE", S#state.queue, 0, -1]),
  {reply, lists:reverse([binary_to_integer(N) || N <- Numbers]), S};
handle_call(pop_all, _From, #state{} = S) ->
  [{ok, Numbers}, _] = eredis:qp(S#state.redis, [["LRANGE", S#state.queue, 0, -1], ["DEL", S#state.queue]]),
  {reply, lists:reverse([binary_to_integer(N) || N <- Numbers]), S};
handle_call(get_primes, _From, #state{} = S) ->
  {ok, Primes} = eredis:q(S#state.redis, ["SMEMBERS", S#state.primes]),
  {reply, [binary_to_integer(P) || P <- Primes], S};
handle_call(flush_primes, _From, #state{} = S) ->
  eredis:q(S#state.redis, ["DEL", S#state.primes]),
  {reply, ok, S}.
