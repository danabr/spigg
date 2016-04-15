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

-type side_effect() :: {line(), local|mfa(), spigg:side_effect_type()}.

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
  case side_effects(Fns, MFA, [], []) of
    {[], [MFA]} -> {error, not_found};
    {_, _}=Res  -> {ok, Res}
  end.

side_effects(Fns, MFA, Unknowns, Seen) ->
  case ordsets:is_element(MFA, Seen) of
    true  -> {[], Unknowns};
    false ->
      case maps:find(MFA, Fns) of
        error                                               ->
          {[], ordsets:add_element(MFA, Unknowns)};
        {ok, #function{calls=Calls, native_side_effects=S}} ->
          SideEffects = lists:map(fun({Line, Effect}) -> {Line, local, Effect} end,
                                  S),
          Seen1 = ordsets:add_element(MFA, Seen),
          lists:foldl(fun({Line, RMFA}, {S0, U0}) ->
            {S1, U1} = side_effects(Fns, RMFA, U0, Seen1),
            S2 = lists:foldl(fun({_, _, E}, SX) ->
              ordsets:add_element({Line, RMFA, E}, SX)
            end, S0, S1),
            {S2, U1}
          end, {SideEffects, Unknowns}, Calls)
      end
  end.
