-module(spigg_analyze_tests).

-include("spigg.hrl").
-include_lib("eunit/include/eunit.hrl").

analyze_not_found_test() ->
  ?assertEqual({error, not_found}, spigg_analyze:beam("not_found")).

analyze_minimal_test() ->
  Expected = #db { functions = #{} },
  Fixture = "test/ebin/minimal.beam",
  ?assertEqual({ok, Expected}, spigg_analyze:beam(Fixture)).

analyze_pure_test() ->
  {ok, DB} = spigg_analyze:beam("test/ebin/pure.beam"),
  assert_side_effects([], DB, pure, add, 2),
  assert_calls([], DB, pure, add, 2),
  assert_side_effects([], DB, pure, reverse, 1),
  assert_calls([{lists, reverse, 1}], DB, pure, reverse, 1),
  assert_side_effects([], DB, pure, even, 1),
  assert_calls([{pure, odd, 1}], DB, pure, even, 1),
  assert_side_effects([], DB, pure, odd, 1),
  assert_calls([{pure, even, 1}], DB, pure, odd, 1),
  assert_side_effects([], DB, pure, sum, 1),
  assert_calls([{pure, sum, 1}], DB, pure, sum, 1),
  assert_side_effects([], DB, pure, exists, 2),
  assert_calls([{lists, any, 2}], DB, pure, exists, 2),
  assert_side_effects([], DB, pure, complex, 2),
  assert_calls([{pure, add, 2}, {pure, reverse, 1}],
               DB, pure, complex, 2),
  assert_side_effects([], DB, pure, erlang_apply, 3),
  assert_calls([{erlang, apply, 3}], DB, pure, erlang_apply, 3),
  assert_side_effects([], DB, pure, imported_apply, 3),
  assert_calls([{erlang, apply, 3}], DB, pure, imported_apply, 3),
  assert_side_effects([], DB, pure, dynamic_mod, 2),
  assert_calls([{erlang, apply, 3}], DB, pure, dynamic_mod, 2),
  assert_side_effects([], DB, pure, dynamic_function, 2),
  assert_calls([{erlang, apply, 3}], DB, pure, dynamic_function, 2),
  assert_side_effects([], DB, pure, dynamic_all, 3),
  assert_calls([{erlang, apply, 3}], DB, pure, dynamic_all, 3),
  assert_num_functions(12, DB).

analyze_side_effects_test() ->
  {ok, DB} = spigg_analyze:beam("test/ebin/side_effects.beam"),
  assert_side_effects(['msg_receive'], DB, side_effects, ping_pong, 0),
  assert_side_effects(['msg_receive'], DB, side_effects, ping_pong_indirect, 0),
  assert_side_effects(['msg_receive'], DB, side_effects, timeout, 0),
  assert_side_effects(['msg_receive'], DB, side_effects, timeout_indirect, 0).

analyze_side_effects_order_test() ->
  {ok, #db{functions=Funs}} = spigg_analyze:beam("test/ebin/side_effects.beam"),
  MFA = {side_effects, side_effect_order, 0},
  #function{native_side_effects=Effects} = maps:get(MFA, Funs),
  Expected = [{13, msg_receive}, {15, msg_receive}],
  ?assertEqual(Expected, Effects).

analyze_self_test() ->
  TestF = fun(Beam, ok) ->
    ?assertMatch({ok, _}, spigg_analyze:beam(Beam))
  end,
  filelib:fold_files("ebin", ".beam", false, TestF, ok).

%% Assert helpers
assert_side_effects(Expected, #db{functions=Funs}, M, F, A) ->
  #function{native_side_effects=Effects} = maps:get({M, F, A}, Funs),
  E = [Type || {_Line, Type} <- Effects],
  ?assertEqual(lists:sort(Expected), E).

assert_calls(Expected, #db{functions=Funs}, M, F, A) ->
  #function{calls=Calls} = maps:get({M, F, A}, Funs),
  MFAs = [MFA || {_, MFA} <- Calls],
  ?assertEqual(lists:sort(Expected), lists:sort(MFAs)).

assert_num_functions(Expected, #db{functions=Funs}) ->
  ?assertEqual(Expected, maps:size(Funs)).
