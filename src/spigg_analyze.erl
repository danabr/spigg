-module(spigg_analyze).

-export([ beam/1
        , forms/1
        ]).

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
%%
%% Q: How to deal with higher order functions? e.g. lists:map?
%% e.g. a function may be pure in itself, if the fun(s) passed
%% to it is pure. Also, a function may construct a fun with a
%% side effect and just return it.
%% A: Dodge the question for now.

-record(mod_data, { name = undefined :: module()
                  , imports = #{} :: #{{atom(), arity()} => module()}
                  , exports = ordsets:new() :: ordsets:ordset(mfa())
                  , raw_functions = [] :: [erl_parse:abstract_form()]
                  }).

-spec beam(Path::string()) -> {ok, spigg:db()} |
                              {error, not_found}.
beam(Path) when is_list(Path) ->
  case beam_lib:chunks(Path, [abstract_code]) of
    {ok, {_Mod, [{abstract_code, {raw_abstract_v1, Code}}]}} ->
      forms(Code);
    {error, beam_lib, {file_error, _Path, enoent}}           ->
      {error, not_found}
  end.

-spec forms([erl_parse:abstract_form()]) -> {ok, spigg:db()}.
forms(Forms) when is_list(Forms) ->
  ModData = analyze_module(Forms),
  RawFunctions = ModData#mod_data.raw_functions,
  Functions = analyze_local(RawFunctions, ModData, []),
  {ok, #db{functions = Functions}}.

analyze_module(Forms) ->
  analyze_module(Forms, #mod_data{}).

analyze_module([], ModData)                                      -> ModData;
analyze_module([{attribute, _Line, module, ModName}|Rest],
               #mod_data{name=undefined}=ModData)                ->
  analyze_module(Rest, ModData#mod_data{name=ModName});
analyze_module([{attribute, _Line, import, {Mod, FAs}}|Rest],
               #mod_data{imports=OldImports}=ModData)               ->
  NewImports = maps:from_list([{FA, Mod} || FA <- FAs]),
  Imports = maps:merge(OldImports, NewImports),
  analyze_module(Rest, ModData#mod_data{imports=Imports});
analyze_module([{attribute, _Line, _Key, _Value}|Rest], ModData) ->
  analyze_module(Rest, ModData);
analyze_module([{eof, _Line}|Rest], ModData)                     ->
  analyze_module(Rest, ModData);
analyze_module([{function, _, _, _, _}=Function|Rest],
               #mod_data{raw_functions=Functions}=ModData)       ->
  analyze_module(Rest, ModData#mod_data{raw_functions=[Function|Functions]}).

analyze_local([], _ModData, Funs)                                         ->
  maps:from_list(Funs);
analyze_local([{function, _Line, Name, Arity, Code}|Rest], ModData, Funs) ->
  {SideEffects, Calls} = analyze_code(Code, ModData, [], []),
  F = #function { calls = Calls
                , native_side_effects = SideEffects
                },
  MFA = {ModData#mod_data.name, Name, Arity},
  analyze_local(Rest, ModData, [{MFA, F}|Funs]).

analyze_code([], _ModData, SideEffects, Calls)                                ->
  {SideEffects, Calls};
analyze_code([{atom, _Line, _Val}|Code], ModData, SideEffects, Calls)         ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([ {call, Line, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}
             | Code], ModData, SideEffects, Calls)                            ->
  % Fully qualified function call
  Call = {Line, {Mod, Fun, length(Args)}},
  analyze_code(Args ++ Code, ModData, SideEffects, [Call|Calls]);
analyze_code([{call, Line, {atom, _, Fun}, Args}|Code],
             ModData, SideEffects, Calls)                                     ->
  Arity = length(Args),
  Mod = identify_source_module(ModData, Fun, Arity),
  Call = {Line, {Mod, Fun, Arity}},
  analyze_code(Args ++ Code, ModData, SideEffects, [Call|Calls]);
analyze_code([{'case', _Line, Expr, Clauses}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code([Expr|Clauses] ++ Code, ModData, SideEffects, Calls);
analyze_code([{clause, _Line, _Args, _Guards, Code}|Clauses],
             ModData, SideEffects, Calls)                                     ->
  %% Guards are side effect free by design, so we disregard them completely.
  analyze_code(Code ++ Clauses, ModData, SideEffects, Calls);
analyze_code([{cons, _Line, HeadExpr, TailExpr}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code([HeadExpr, TailExpr|Code], ModData, SideEffects, Calls);
%% Note: a fun is only part of the enclosing function if
%% is called from the function. We can't reliably tell if
%% that happens with the local view we have.
analyze_code([{'fun', _Line, {clauses, _Clauses}}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{'fun', _Line, {function, _F, _A}}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{integer, _Line, _Val}|Code], ModData, SideEffects, Calls)      ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{generate, _Line, Lhs, Rhs}|Code], ModData, SideEffects, Calls) ->
  analyze_code([Lhs, Rhs|Code], ModData, SideEffects, Calls);
analyze_code([{lc, _Line, Lhs, Generators}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code([Lhs|Generators] ++ Code, ModData, SideEffects, Calls);
analyze_code([{map, _Line, Elements}|Code], ModData, SideEffects, Calls)      ->
  analyze_code(Elements ++ Code, ModData, SideEffects, Calls);
analyze_code([{match, _Line, _Lhs, Rhs}|Code], ModData, SideEffects, Calls)   ->
  analyze_code([Rhs|Code], ModData, SideEffects, Calls);
analyze_code([{nil, _Line}|Code], ModData, SideEffects, Calls)                ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{op, _, _Op, Expr}|Code], ModData, SideEffects0, Calls0)        ->
  {SideEffects, Calls} = analyze_code(listify(Expr), ModData,
                                      SideEffects0, Calls0),
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{op, _, _Op, Lhs, Rhs}|Code],
             ModData, SideEffects0, Calls0)                                   ->
  {SideEffects1, Calls1} = analyze_code(listify(Lhs), ModData,
                                        SideEffects0, Calls0),
  {SideEffects2, Calls2} = analyze_code(listify(Rhs), ModData,
                                        SideEffects1, Calls1),
  analyze_code(Code, ModData, SideEffects2, Calls2);
analyze_code([{record, _Line, {var, _, _Var}, _Name, Fields}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code(Fields++Code, ModData, SideEffects, Calls);
analyze_code([{record, _Line, _Name, Fields}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code(Fields++Code, ModData, SideEffects, Calls);
analyze_code([{record_field, _Line, {atom, _Line, _Field}, Rhs}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code([Rhs|Code], ModData, SideEffects, Calls);
analyze_code([{record_field, _Line, {var, _Line, _Var},
              _Record, {atom, _Line, _Field}}|Code],
             ModData, SideEffects, Calls)                                     ->
  analyze_code(Code, ModData, SideEffects, Calls);
analyze_code([{tuple, _Line, Elements}|Code], ModData, SideEffects, Calls)    ->
  analyze_code(Elements++Code, ModData, SideEffects, Calls);
analyze_code([{var, _Line, _}|Code], ModData, SideEffects, Calls)             ->
  analyze_code(Code, ModData, SideEffects, Calls).

listify(Expressions) when is_list(Expressions) -> Expressions;
listify(Expression)                            -> [Expression].

identify_source_module(#mod_data{name=Name, raw_functions=Raw,
                                 imports=Imports}, Fun, Arity) ->
  IsLocalF = fun({function, _Line, F, A, _Code})   ->
                F =:= Fun andalso A =:= Arity
             end,
  case lists:any(IsLocalF, Raw) of
    true  -> Name;
    false ->
      case maps:find({Fun, Arity}, Imports) of
        {ok, Mod} -> Mod;
        error     -> erlang
      end
  end.
