-module(spigg).

-export([ add_function/4
        , merge/2
        , new_db/0
        , side_effects/2
        ]).

-include("spigg.hrl").

-type db() :: #db{}.

-type func() :: #function{}.

-type line() :: non_neg_integer().

-type call() :: {line(), mfa()}.

-type native_side_effect() :: {line(), spigg:side_effect_type()}.

-type side_effect() :: {line(), Trace::[mfa()], spigg:side_effect_type()}.

-type side_effect_type() :: 'send'.

-export_type([ db/0
             , func/0
             , call/0
             , native_side_effect/0
             , side_effect/0
             , side_effect_type/0
             ]).

-spec add_function(spigg:db(), mfa(), [spigg:call()],
                   [spigg:native_side_effect()]) -> spigg:db().
%% @doc Add function MFA to DB.
%% TODO: If MFA already exists in DB, it will be replaced with the new information.
add_function(#db{functions=Fns}=DB, MFA, Calls, SideEffects) ->
  F = #function { calls = Calls
                , native_side_effects=ordsets:from_list(SideEffects)
                },
  DB#db{functions=maps:put(MFA, F, Fns)}.

-spec merge(Base::spigg:db(), Override::spigg:db()) -> spigg:db().
merge(#db{functions=Old}, #db{functions=New}) ->
  #db{functions=maps:merge(Old, New)}.

-spec new_db() -> spigg:db().
%% @doc Initialize a new side effect database.
new_db() -> #db{}.

-spec side_effects(spigg:db(), mfa()) -> {error, not_found} |
                                         {ok, {SideEffects, Unknowns}}
  when SideEffects :: ordsets:ordset(spigg:side_effect()),
       Unknowns :: ordsets:ordset(mfa()).
%% @doc Lookup side effects of MFA.
side_effects(#db{functions=Fns}, MFA) ->
  case side_effects(Fns, MFA, [], #{}) of
    {[], [MFA], _}                 -> {error, not_found};
    {SideEffects, Unknown, _Seen}  -> {ok, {SideEffects, Unknown}}
  end.

%% Fns :: Our knowledge of the world
%% MFA :: The MFA we are analyzing
%% Unknowns :: Unknown MFAs so far
%% Seen :: MFAs we have already calculated the side effects for
side_effects(Fns, MFA, Unknowns, Seen) ->
  case maps:find(MFA, Seen) of
    {ok, SideEffects}  ->
      {SideEffects, Unknowns, Seen};
    error              ->
      case maps:find(MFA, Fns) of
        error    -> {[], ordsets:add_element(MFA, Unknowns), Seen};
        {ok, Fn} -> calculate_side_effects(Fns, MFA, Unknowns, Seen, Fn)
      end
  end.

calculate_side_effects(Fns, MFA, Unknowns0, Seen0, #function{calls=Calls}=F) ->
  LocalSideEffects = local_side_effects(F),
  SeenSelf = maps:put(MFA, LocalSideEffects, Seen0),
  {AllSideEffects, Unknown, Seen} =
    add_side_effects_from_calls(Fns, Unknowns0, SeenSelf, LocalSideEffects,
                                Calls),
  {AllSideEffects, Unknown, maps:put(MFA, AllSideEffects, Seen)}.

local_side_effects(#function{native_side_effects=S}) ->
  lists:map(fun({Line, Effect}) -> {Line, [], Effect} end, S).

add_side_effects_from_calls(_Fns, Unknowns, Seen, SideEffects, []) ->
  {SideEffects, Unknowns, Seen};
add_side_effects_from_calls(Fns, Unknowns0, Seen0, SideEffects0,
                            [{Line, MFA}|Calls])                   ->
  {CallSideEffects, Unknowns, Seen} = side_effects(Fns, MFA, Unknowns0, Seen0),
  SideEffects = add_side_effects(Line, MFA, SideEffects0, CallSideEffects),
  add_side_effects_from_calls(Fns, Unknowns, Seen, SideEffects, Calls).

add_side_effects(Line, MFA, OldSideEffects, NewSideEffects) ->
  lists:foldl(fun({_, MFAs, Effect}, CurrentSideEffects) ->
    ordsets:add_element({Line, [MFA|MFAs], Effect}, CurrentSideEffects)
  end, OldSideEffects, NewSideEffects).
