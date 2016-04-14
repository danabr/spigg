-module(side_effects).

-export([ping_pong/0
       , timeout/0
       ]).

ping_pong() ->
  self() ! ping,
  receive
    ping -> pong
  end.

timeout() ->
  receive
  after
    1000 -> timeout
  end.


