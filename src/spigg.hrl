-ifndef(__SPIGG_HRL).
-define(__SPIGG_HRL, true).

-record(db, { modules = #{} :: #{atom() => spigg:mod()}
            , functions = #{} :: #{mfa() => spigg:func()}
            }).

-record(module, { dependencies = ordsets:new() :: ordsets:ordset(module())
                }).

-record(function, { exported=erlang:error(missing_field) :: boolean()
                  , side_effects = [] :: [spigg:side_effect()]
                  , unknowns = [] :: [{non_neg_integer(), mfa()}]
                  }).

-record(side_effect, { type=error(missing_field) :: spigg:side_effect_type()
                     , line=error(missing_field) :: non_neg_integer()
                     , origin=error(missing_field) :: mfa()
                     }).
-endif.
