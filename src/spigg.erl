-module(spigg).

-export([ new_db/0
        ]).

-include("spigg.hrl").

-opaque db() :: #db{}.

-type func() :: #function{}.

-type line() :: non_neg_integer().

-type call() :: {line(), mfa()}.

-type side_effect() :: {line(), spigg:side_effect_type()}.

-type side_effect_type() :: 'send'.

-export_type([ db/0
             , func/0
             , call/0
             , side_effect/0
             , side_effect_type/0
             ]).

-spec new_db() -> spigg:db().
new_db() -> #db{}.
