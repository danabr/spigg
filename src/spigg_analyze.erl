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

-type line() :: non_neg_integer().

-record(mod_data, { name = undefined :: module()
                  , imports = ordsets:new() :: ordsets:ordset(mfa())
                  , exports = ordsets:new() :: ordsets:ordset(mfa())
                  , raw_functions = [] :: [erl_parse:abstract_form()]
                  }).

-record(local_function, { name=error(missing_field) :: atom()
                        , arity=error(missing_field) :: non_neg_integer()
                        , exported=error(missing_field) :: boolean()
                        , calls=[] :: [{line(), mfa()}]
                        , local_side_effects=[] ::
                            [{line(), spigg:side_effect_type()}]
                        }).

-spec forms([erl_parse:abstract_form()]) -> {ok, spigg:db()}.
forms(Forms) when is_list(Forms) ->
  ModData = analyze_module(Forms),
  RawFunctions = ModData#mod_data.raw_functions,
  ModName =  ModData#mod_data.name,
  LocalFunctions = analyze_local(RawFunctions, ModData, []),
  Dependencies = module_dependencies(LocalFunctions, ModName),
  Module = #module{dependencies = Dependencies},
  Modules = maps:from_list([{ModName, Module}]),
  Functions0 = local_functions_to_spigg_functions(LocalFunctions, ModName),
  Functions = propagate_side_effects(Functions0),
  {ok, #db{functions = Functions, modules = Modules}}.

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
  Funs;
analyze_local([{function, _Line, Name, Arity, Code}|Rest], ModData, Funs) ->
  {SideEffects, Calls} =
    analyze_function(Code, {Name, Arity}, ModData, [], []),
  F = #local_function { name = Name
                      , arity = Arity
                      , exported = is_exported(ModData, Name, Arity)
                      , calls = Calls
                      , local_side_effects = SideEffects
                      },
  analyze_local(Rest, ModData, [F|Funs]).

is_exported(#mod_data{exports=Exports, name=Mod}, Fun, Arity) ->
  ordsets:is_element({Mod, Fun, Arity}, Exports).

analyze_function([], _Id, _ModData, SideEffects, Calls) ->
  {SideEffects, Calls};
analyze_function([{clause, _Line, _Args, _Guards, Code}|Clauses],
                 Id, ModData, SideEffects0, Calls0)     ->
  %% Guards are side effect free by design, so we disregard them completely.
  {SideEffects, Calls} = analyze_code(Code, Id, ModData, SideEffects0, Calls0),
  analyze_function(Clauses, Id, ModData, SideEffects, Calls).

analyze_code([], _Id, _ModData, SideEffects, Calls)                        ->
  {SideEffects, Calls};
analyze_code([ {call, Line, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}
             | Code], Id, ModData, SideEffects, Calls)                     ->
  % Fully qualified function call
  Call = {Line, {Mod, Fun, length(Args)}},
  analyze_code(Code, Id, ModData, SideEffects, [Call|Calls]);
analyze_code([{call, Line, {atom, _, Fun}, Args}|Code],
             Id, ModData, SideEffects, Calls)                              ->
  Arity = length(Args),
  Mod = identify_source_module(ModData, Fun, Arity),
  Call = {Line, {Mod, Fun, Arity}},
  analyze_code(Code, Id, ModData, SideEffects, [Call|Calls]);
analyze_code([{op, _, _Op, Expr}|Code], Id, ModData, SideEffects0, Calls0) ->
  {SideEffects, Calls} = analyze_code(listify(Expr), Id, ModData,
                                      SideEffects0, Calls0),
  analyze_code(Code, Id, ModData, SideEffects, Calls);
analyze_code([{op, _, _Op, Lhs, Rhs}|Code],
             Id, ModData, SideEffects0, Calls0)                            ->
  {SideEffects1, Calls1} = analyze_code(listify(Lhs), Id, ModData,
                                        SideEffects0, Calls0),
  {SideEffects2, Calls2} = analyze_code(listify(Rhs), Id, ModData,
                                        SideEffects1, Calls1),
  analyze_code(Code, Id, ModData, SideEffects2, Calls2);
analyze_code([{atom, _Line, _Atom}|Code], Id, ModData, SideEffects, Calls) ->
  analyze_code(Code, Id, ModData, SideEffects, Calls);
analyze_code([{var, _Line, _}|Code], Id, ModData, SideEffects, Calls)      ->
  analyze_code(Code, Id, ModData, SideEffects, Calls).

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

module_dependencies(LocalFuns, ModName) ->
  ordsets:del_element(ModName, module_dependencies(LocalFuns)).

module_dependencies(LocalFuns) ->
  lists:foldl(fun(#local_function{calls=Calls}, Deps0) ->
                lists:foldl(fun({_Line, {M, _F, _A}}, Deps) ->
                              ordsets:add_element(M, Deps)
                            end, Deps0, Calls)
              end, ordsets:new(), LocalFuns).

local_functions_to_spigg_functions(LocalFuns, ModName) ->
  maps:from_list(lists:map(fun(Fun) ->
    #local_function{name=Name, arity=Arity} = Fun,
    MFA = {ModName, Name, Arity},
    LocalEffects = Fun#local_function.local_side_effects,
    SideEffects = local_side_effects(LocalEffects, MFA),
    {MFA, #function{ exported = Fun#local_function.exported
                   , side_effects = SideEffects
                   , unknowns = Fun#local_function.calls
                   }}
  end, LocalFuns)).

local_side_effects(Local, MFA) ->
  lists:map(fun({Line, Type}) ->
              #side_effect{ type=Type, line=Line, origin=MFA}
            end, Local).

propagate_side_effects(Functions) ->
  case do_propagate_side_effects(Functions) of
    Functions         -> Functions; % Fixpoint
    UpdatedFunctions -> propagate_side_effects(UpdatedFunctions)
  end.

do_propagate_side_effects(Functions) ->
  maps:map(fun(_MFA, Fun) ->
    lookup_unknowns(Fun, Functions)
  end, Functions).

lookup_unknowns(#function{side_effects=Effects0, unknowns=Unknowns0}=Function,
                World) ->
  {Effects, Unknowns} = lookup_unknowns(Unknowns0, World, [], []),
  Function#function{side_effects=Effects0 ++ Effects, unknowns=Unknowns}.

lookup_unknowns([], _World, NewSideEffects, NewUnknowns) ->
  {NewSideEffects, lists:reverse(NewUnknowns)};
lookup_unknowns([{Line, MFA}=Unknown|Unknowns], World,
                SideEffects0, NewUnknowns)               ->
  case maps:find(MFA, World) of
    error               ->
      lookup_unknowns(Unknowns, World, SideEffects0, [Unknown|NewUnknowns]);
    {ok, OtherFunction} ->
      NewEffects = OtherFunction#function.side_effects,
      SideEffects = unknown_to_side_effects(NewEffects, Line, MFA),
      lookup_unknowns(Unknowns, World, SideEffects++SideEffects0, NewUnknowns)
  end.

unknown_to_side_effects(SideEffects, Line, MFA) ->
  lists:map(fun(#side_effect{type=Type}) ->
              #side_effect{type=Type, origin=MFA, line=Line}
            end, SideEffects).
