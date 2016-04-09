-module(spigg_analyze_tests).

-include("spigg.hrl").
-include_lib("eunit/include/eunit.hrl").

analyze_minimal_test() ->
  Expected = #db { functions = #{}
                 , modules = #{minimal => #module{}}
                 },
  {ok, Forms} = load_fixture("test/fixtures/minimal.erl"),
  ?assertEqual({ok, Expected}, spigg_analyze:forms(Forms)).

analyze_pure_test() ->
  {ok, Forms} = load_fixture("test/fixtures/pure.erl"),
  {ok, DB} = spigg_analyze:forms(Forms),
  assert_modules([pure], DB),
  assert_dependencies([lists], DB, pure),
  assert_side_effects([], DB, pure, add, 2),
  assert_unknowns([], DB, pure, add, 2),
  assert_side_effects([], DB, pure, reverse, 1),
  assert_unknowns([{lists, reverse, 1}], DB, pure, reverse, 1),
  assert_side_effects([], DB, pure, even, 1),
  assert_unknowns([], DB, pure, even, 1),
  assert_side_effects([], DB, pure, odd, 1),
  assert_unknowns([], DB, pure, odd, 1),
  assert_num_functions(4, DB).

%% Assert helpers
assert_modules(Expected, #db{modules=Mods}) ->
  ?assertEqual(lists:sort(Expected), lists:sort(maps:keys(Mods))).

assert_dependencies(Expected, #db{modules=Mods}, Mod) ->
  #module{dependencies=Actual} = maps:get(Mod, Mods),
  ?assertEqual(ordsets:from_list(Expected), Actual).

assert_side_effects(Expected, #db{functions=Funs}, M, F, A) ->
  #function{side_effects=Effects} = maps:get({M, F, A}, Funs),
  E = [{Type, Origin} || #side_effect{type=Type, origin=Origin} <- Effects],
  ?assertEqual(lists:sort(Expected), lists:sort(E)).

assert_unknowns(Expected, #db{functions=Funs}, M, F, A) ->
  #function{unknowns=Unknowns} = maps:get({M, F, A}, Funs),
  MFAs = [MFA || {_, MFA} <- Unknowns],
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
