-module(spigg_tests).

-include_lib("eunit/include/eunit.hrl").

side_effects_not_found_test() ->
  DB = spigg:new_db(),
  ?assertEqual({error, not_found}, spigg:side_effects(DB, {foo, bar, 1})).

no_side_effect_test() ->
  MFA = {foo, bar, 1},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, MFA, [], []),
  ?assertEqual({ok, {complete, []}}, spigg:side_effects(DB1, MFA)).

simple_side_effect_test() ->
  MFA = {foo, bar, 1},
  DB0 = spigg:new_db(),
  SideEffects = [{14, 'send'}],
  DB1 = spigg:add_function(DB0, MFA, [], SideEffects),
  Expected = [{14, local, 'send'}],
  ?assertEqual({ok, {complete, Expected}}, spigg:side_effects(DB1, MFA)).

no_side_effect_recursion_test() ->
  MFA = {foo, bar, 1},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, MFA, [{12, MFA}], []),
  ?assertEqual({ok, {complete, []}}, spigg:side_effects(DB1, MFA)).

no_side_effect_mutual_recursion_test() ->
  Even = {foo, bar, 1},
  Odd = {bar, baz, 2},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, Even, [{1, Odd}], []),
  DB2 = spigg:add_function(DB1, Odd, [{2, Even}], []),
  ?assertEqual({ok, {complete, []}}, spigg:side_effects(DB2, Even)),
  ?assertEqual({ok, {complete, []}}, spigg:side_effects(DB2, Odd)).

incomplete_side_effect_test() ->
  MFA = {foo, bar, 1},
  DB0 = spigg:new_db(),
  Calls = [{12, {bar, baz, 2}}],
  SideEffects = [{14, 'send'}],
  DB1 = spigg:add_function(DB0, MFA, Calls, SideEffects),
  Expected = [{14, local, 'send'}], 
  ?assertEqual({ok, {incomplete, Expected}}, spigg:side_effects(DB1, MFA)).

two_level_side_effect_test() ->
  MFA = {foo, bar, 1},
  RemoteMFA = {bar, baz, 2},
  DB0 = spigg:new_db(),
  Calls = [{12, RemoteMFA}],
  SideEffects = [{14, 'send'}],
  DB1 = spigg:add_function(DB0, MFA, Calls, SideEffects),
  DB2 = spigg:add_function(DB1, RemoteMFA, [], [{95, 'receive'}]),
  Expected = [{12, RemoteMFA, 'receive'}, {14, local, 'send'}],
  ?assertEqual({ok, {complete, Expected}}, spigg:side_effects(DB2, MFA)).

three_level_side_effect_test() ->
  MFA = {foo, bar, 1},
  RemoteMFA = {bar, baz, 2},
  DeepMFA = {baz, oogle, 3},
  DB0 = spigg:new_db(),
  SideEffects = [{14, 'send'}],
  DB1 = spigg:add_function(DB0, MFA, [{12, RemoteMFA}], SideEffects),
  DB2 = spigg:add_function(DB1, RemoteMFA, [{8,DeepMFA}], [{95, 'receive'}]),
  DB3 = spigg:add_function(DB2, DeepMFA, [], [{4, 'time'}]),
  Expected = [ {12, RemoteMFA, 'receive'}
             , {12, RemoteMFA, 'time'}
             , {14, local, 'send'}  
             ],
  ?assertEqual({ok, {complete, Expected}}, spigg:side_effects(DB3, MFA)).

mutual_recursion_side_effect_test() ->
  Even = {foo, bar, 1},
  Odd = {bar, baz, 2},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, Even, [{1, Odd}], []),
  DB2 = spigg:add_function(DB1, Odd, [{2, Even}], [{1, time}]),
  ?assertEqual({ok, {complete, [{1, Odd, time}]}},
               spigg:side_effects(DB2, Even)),
  ?assertEqual({ok, {complete, [{1, local, time}]}},
               spigg:side_effects(DB2, Odd)).

multipe_calls_side_effect_test() ->
  Caller = {foo, bar, 1},
  Callee = {bar, baz, 2},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, Caller, [{1, Callee}, {2, Callee}], []),
  DB2 = spigg:add_function(DB1, Callee, [], [{1, time}]),
  ?assertEqual({ok, {complete, [{1, Callee, time}, {2, Callee, time}]}},
               spigg:side_effects(DB2, Caller)).

multiple_deep_calls_side_effect_test() ->
  Top = {foo, bar, 1},
  Caller = {bar, baz, 2},
  Callee = {baz, oogle, 3},
  DB0 = spigg:new_db(),
  DB1 = spigg:add_function(DB0, Top, [{1, Caller}, {2, Callee}, {3, Caller}],
                           []),
  DB2 = spigg:add_function(DB1, Caller, [{1, Callee}, {2, Callee}], []),
  DB3 = spigg:add_function(DB2, Callee, [], [{1, time}]),
  ?assertEqual({ok, {complete, [ {1, Caller, time}
                               , {2, Callee, time}
                               , {3, Caller, time}
                               ]}},
               spigg:side_effects(DB3, Top)).
