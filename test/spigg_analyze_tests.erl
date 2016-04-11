-module(spigg_analyze_tests).

-include("spigg.hrl").
-include_lib("eunit/include/eunit.hrl").

analyze_minimal_test() ->
  Expected = #db { functions = #{}
                 , dependencies = #{}
                 },
  {ok, Forms} = load_fixture("test/fixtures/minimal.erl"),
  ?assertEqual({ok, Expected}, spigg_analyze:forms(Forms)).

analyze_pure_test() ->
  {ok, Forms} = load_fixture("test/fixtures/pure.erl"),
  {ok, DB} = spigg_analyze:forms(Forms),
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
  assert_num_functions(5, DB),
  assert_dependencies(#{ {lists, reverse, 1} => [{pure,reverse, 1}]
                       , {pure, even, 1}     => [{pure, odd, 1}]
                       , {pure, odd, 1}      => [{pure, even, 1}]
                       }, DB).

%% Assert helpers
assert_dependencies(Expected, #db{dependencies=Actual}) ->
  ?assertEqual(Expected, Actual).

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

%% Test helpers
load_fixture(File) ->
  {ok, Fixture} = file:read_file(File),
  parse(erl_scan:tokens([], binary_to_list(Fixture), 1)).

parse(ScanRes) ->
  parse(ScanRes, []).

parse([], Forms)                    -> {ok, lists:reverse(Forms)};
parse(Str, Forms) when is_list(Str) ->
  parse(erl_scan:tokens([], Str, 1), Forms);
parse({done, Res, Rest}, Forms)     ->
  parse_done(Res, Rest, Forms).

parse_done({ok, Tokens, _Location}, Rest, Forms) ->
  case erl_parse:parse_form(Tokens) of
    {ok, Form} -> parse(Rest, [Form|Forms]);
    Err        -> Err
  end.
