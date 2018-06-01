-module(funbox_utils).
-import(erlang, [send_after/3, monotonic_time/1, cancel_timer/1]).

-export([init_periodic_message/1, continue_periodic_message/4]).

init_periodic_message(Message) ->
  {send_after(0, self(), Message), monotonic_time(millisecond)}.

continue_periodic_message(Message, Delay, StartTime, TimerRef) ->
  cancel_timer(TimerRef),
  RealDelay = Delay - (monotonic_time(millisecond) - StartTime) rem Delay,
  send_after(RealDelay, self(), Message).