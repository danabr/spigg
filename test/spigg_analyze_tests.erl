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
  assert_num_functions(5, DB).

analyze_self_test() ->
  TestF = fun(Beam, ok) ->
    ?debugFmt("analyze_self_test: ~p", [Beam]),
    ?assertMatch({ok, _}, spigg_analyze:beam(Beam))
  end,
  filelib:fold_files("ebin", ".beam", false, TestF, ok).

%% Assert helpers
assert_side_effects(Expected, #db{functions=Funs}, M, F, A) ->
  #function{native_side_effects=Effects} = maps:get({M, F, A}, Funs),
  E = [Type || {_Line, Type} <- Effects],
  ?assertEqual(lists:sort(Expected), lists:sort(E)).

assert_calls(Expected, #db{functions=Funs}, M, F, A) ->
  #function{calls=Calls} = maps:get({M, F, A}, Funs),
  MFAs = [MFA || {_, MFA} <- Calls],
  ?assertEqual(lists:sort(Expected), lists:sort(MFAs)).

assert_num_functions(Expected, #db{functions=Funs}) ->
  ?assertEqual(Expected, maps:size(Funs)).
