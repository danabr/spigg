-module(spigg_analyze).

-export([forms/1]).

-include("spigg.hrl").

%% Q: How should we deal with the implicit import of the erlang module?
%% A: All unqualified calls that do not refer to a local function or an
%% imported function, can be regarded as calls to the erlang module.
%%
%% Q: Why not analyze erl files directly?
%% A: When doing so, you have to deal with:
%%   * macros
%%   * parse transforms
%%   * include files

-record(mod_data, { name = undefined :: module()
                  , imports = ordsets:new() :: ordsets:ordset(mfa())
                  , exports = ordsets:new() :: ordsets:ordset(mfa())
                  , raw_functions = [] :: [erl_parse:abstract_form()]
                  }).

-spec forms([erl_parse:abstract_form()]) -> {ok, #db{}}.
forms(Forms) when is_list(Forms) ->
  ModData = analyze_module(Forms),
  RawFunctions = ModData#mod_data.raw_functions,
  Functions = analyze_local(RawFunctions, ModData, []),
  Dependencies = dependencies(Functions),
  {ok, #db{functions = Functions, dependencies = Dependencies}}.

analyze_module(Forms) ->
  analyze_module(Forms, #mod_data{}).

analyze_module([], ModData)                                -> ModData;
analyze_module([{attribute, _Line, module, ModName}|Rest],
               #mod_data{name=undefined}=ModData)          ->
  analyze_module(Rest, ModData#mod_data{name=ModName});
analyze_module([{function, _, _, _, _}=Function|Rest],
               #mod_data{raw_functions=Functions}=ModData) ->
  analyze_module(Rest, ModData#mod_data{raw_functions=[Function|Functions]}).

analyze_local([], _ModData, Funs)                                         ->
  maps:from_list(Funs);
analyze_local([{function, _Line, Name, Arity, Code}|Rest], ModData, Funs) ->
  {SideEffects, Calls} = analyze_function(Code, ModData, [], []),
  F = #function { calls = Calls
                , native_side_effects = SideEffects
                },
  MFA = {ModData#mod_data.name, Name, Arity},
  analyze_local(Rest, ModData, [{MFA, F}|Funs]).

analyze_function([], _ModData, SideEffects, Calls) ->
  {SideEffects, Calls};
analyze_function([{clause, _Line, _Args, _Guards, Code}|Clauses],
                 ModData, SideEffects0, Calls0)    ->
  %% Guards are side effect free by design, so we disregard them completely.
  {SideEffects, Calls} = analyze_code(Code, ModData, SideEffects0, Calls0),
  analyze_function(Clauses, ModData, SideEffects, Calls).

analyze_code([], _ModData, SideEffects, Calls)                           ->
  {SideEffects, Calls};
analyze_code([ {call, Line, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}
             | Code], ModData, SideEffects, Calls)                       ->
  % Fully qualified function call
  Call = {Line, {Mod, Fun, length(Args)}},
  analyze_code(Args ++ Code, ModData, SideEffects, [Call|Calls]);
analyze_code([{call, Line, {atom, _, Fun}, Args}|Code],
             ModData, SideEffects, Calls)                                ->
  Arity = length(Args),
  Mod = identify_source_module(ModData, Fun, Arity),
  Call = {Line, {Mod, Fun, Arity}},
  analyze_code(Args ++ Code, ModData, SideEffects, [Call|Calls]);
analyze_code([{op, _, _Op, Expr}|Code], ModData, SideEffects0, Calls0)   ->
  {SideEffects, Calls} = analyze_code(listify(Expr), ModData,
                                      SideEffects0, Calls0),
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{op, _, _Op, Lhs, Rhs}|Code],
             ModData, SideEffects0, Calls0)                              ->
  {SideEffects1, Calls1} = analyze_code(listify(Lhs), ModData,
                                        SideEffects0, Calls0),
  {SideEffects2, Calls2} = analyze_code(listify(Rhs), ModData,
                                        SideEffects1, Calls1),
  analyze_code(Code, ModData, SideEffects2, Calls2);
analyze_code([{atom, _Line, _Val}|Code], ModData, SideEffects, Calls)    ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{integer, _Line, _Val}|Code], ModData, SideEffects, Calls) ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{var, _Line, _}|Code], ModData, SideEffects, Calls)        ->
  analyze_code(Code, ModData, SideEffects, Calls).

listify(Expressions) when is_list(Expressions) -> Expressions;
listify(Expression)                            -> [Expression].

identify_source_module(#mod_data{name=Name, raw_functions=Raw}, Fun, Arity) ->
  IsLocalF = fun({function, _Line, F, A, _Code})   ->
                F =:= Fun andalso A =:= Arity
             end,
  case lists:any(IsLocalF, Raw) of
    true -> Name;
    false -> error({not_local, Fun, Arity, Raw}) % TODO: Check imports
                              % TODO: Fallback to the erlang module
  end.

dependencies(Funs) ->
  maps:fold(fun add_dependencies/3, #{}, Funs).

add_dependencies(Source, #function{calls=Calls}, Deps0) ->
  lists:foldl(fun({_Line, MFA}, Deps) when Source =/= MFA ->
                add_dependency(Source, MFA, Deps);
                 ({_Line, _Source}, Deps)                 -> % recursion
                Deps
              end, Deps0, Calls).

add_dependency(Source, MFA, Deps) ->
  MFADeps = case maps:find(MFA, Deps) of
    {ok, Existing} -> ordsets:add_element(Source, Existing);
    error          -> [Source]
  end,
  maps:put(MFA, MFADeps, Deps).
