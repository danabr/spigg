-module(spigg).

-export([ new_db/0
        ]).

-include("spigg.hrl").

-opaque db() :: #db{}.

-type mod() :: #module{}.

-type func() :: #function{}.

-opaque side_effect() :: #side_effect{}.

-type side_effect_type() :: 'send'.


-export_type([ db/0
             , mod/0
             , func/0
             , side_effect/0
             , side_effect_type/0
             ]).

-spec new_db() -> spigg:db().
new_db() -> #db{}.
