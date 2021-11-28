-module(test_child_app).

-telemetry_event #{
                   event => [test_child_app, extra_long, start],
                   description => <<"Emitted at the start of the handler">>,
                   measurements => <<"#{system_time => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_child_app, extra_long, stop],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.

-telemetry_event #{
                   event => [test_child_app, cache, hit],
                   description => <<"Emitted at the start of the handler">>,
                   measurements => <<"#{system_time => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_child_app, cache, miss],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
