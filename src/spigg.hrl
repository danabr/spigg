-ifndef(__SPIGG_HRL).
-define(__SPIGG_HRL, true).

-record(db, { functions = #{} :: #{mfa() => spigg:func()}
            , dependencies = #{} :: #{mfa() => ordsets:ordset(mfa())}
            }).

-record(function, { native_side_effects = [] ::
                      ordsets:ordset(spigg:native_side_effect())
                  , calls = [] :: [spigg:call()]
                  }).

-endif.
