-module(side_effects).

-export([ping_pong/0
       , ping_pong_indirect/0
       , timeout/0
       , timeout/1
       ]).

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

timeout(Timeout) ->
  receive
  after
    Timeout -> timeout
  end.
