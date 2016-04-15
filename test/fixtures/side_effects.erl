-module(side_effects).

-export([ping_pong/0
       , ping_pong_indirect/0
       , timeout/0
       , timeout_fun/0
       , timeout/1
       , timeout_indirect/0
       , side_effect_order/0
       ]).

side_effect_order() ->
  receive
    one ->
      receive
        two -> ok
      end
    end.

ping_pong() ->
  self() ! ping,
  receive
    ping -> pong
  end.

ping_pong_indirect() ->
  (fun (X) ->
    receive Y -> X + Y end
  end)(1).

timeout() ->
  receive
  after
    1000 -> timeout
  end.

timeout_fun() ->
  fun timeout/0.

timeout(Timeout) ->
  receive
  after
    Timeout -> timeout
  end.

timeout_indirect() ->
  fun Loop() -> receive x -> Loop() end end.
