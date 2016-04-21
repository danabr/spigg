-module(pure).

-import(lists, [any/2]).

-compile([export_all]).

-record(rec, { nested :: #rec{} }).

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

erlang_apply(M, F, Args) ->
  erlang:apply(M, F, Args).

imported_apply(M, F,Args) ->
  apply(M, F, Args).

dynamic_mod(M, Arg) ->
  M:function(Arg).

dynamic_function(F, Arg) ->
  mod:F(Arg).

dynamic_all(M, F, Arg) ->
  M:F(Arg).

complex(F, Arg) ->
  Res = (catch F(Arg)),
  try F of
    add -> fun ?MODULE:add/2;
    reverse -> fun reverse/1;
    dynamic_fun_reference -> fun Arg:F/3
  catch
    _:_ -> error
  after
    cleanup
  end,
  if 
    Res > 1 -> large;
    true    -> small
  end,
  Res = case Arg of
    "string" -> "string";
    <<>>     -> <<>>;
    <<1,2,3>> -> <<"123">>;
    #rec{nested=undefined} -> (F(Res))#rec{nested=Arg};
    #rec{nested=#rec{nested=_}} -> #rec{_='_'};
    #rec{}=R -> (F(R, #rec.nested))#rec.nested;
    #{} -> #{a => F([1,2,3])};
    Map when is_map(Map)-> Map#{ok := inconsistent};
    $D       -> $E;
    1.0      -> 1.0e2;
    Binary when is_binary(Binary) -> << <<X/integer>> || <<X>> <= Binary, F(X) >>
  end,
  Res = begin 1 + 2 end.
