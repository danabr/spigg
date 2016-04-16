-ifndef(__SPIGG_HRL).
-define(__SPIGG_HRL, true).

-record(db, { version=1 :: non_neg_integer()
            , functions = #{} :: #{mfa() => spigg:func()}
            }).

-record(function, { native_side_effects = [] ::
                      ordsets:ordset(spigg:native_side_effect())
                  , calls = [] :: [spigg:call()]
                  }).

-endif.
