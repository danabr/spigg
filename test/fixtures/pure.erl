-module(pure).

-import(lists, [any/2]).

-compile([export_all]).

add(A, B) when is_integer(A), is_integer(B) ->
  A + B.

reverse(L) when is_list(L) ->
  lists:reverse(L).

even(0)            -> true;
even(N) when N > 0 -> not odd(N-1).

odd(1)            -> true;
odd(N) when N > 0 -> not even(N-1).

sum([])       -> 0;
sum([X|Rest]) -> X + sum(Rest).

exists(P, L) when is_function(P, 1), is_list(L) ->
  any(P, L).

call_fun(F, Arg) -> F(Arg).
