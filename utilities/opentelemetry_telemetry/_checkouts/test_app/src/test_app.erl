-module(test_app).

-export([handler/1]).

-telemetry_event #{
                  event => [test_app, handler, start],
                  description => <<"Emitted at the start of the handler">>,
                  measurements => <<"#{system_time => non_neg_integer()}">>,
                  metadata => <<"#{}">>
                 }.
-telemetry_event #{
                   event => [test_app, handler, stop],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_app, handler, exception],
                   description => <<"The handler raised an exception">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{kind => atom(), reason => atom(), stacktrace => term()}">>
                  }.

-telemetry_event #{
                  event => [test_app, nested_span, start],
                  description => <<"Emitted at the start of the handler">>,
                  measurements => <<"#{system_time => non_neg_integer()}">>,
                  metadata => <<"#{}">>
                 }.
-telemetry_event #{
                   event => [test_app, nested_span, stop],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.
-telemetry_event #{
                   event => [test_app, nested_span, exception],
                   description => <<"The handler raised an exception">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{kind => atom(), reason => atom(), stacktrace => term()}">>
                  }.


-telemetry_event #{
                   event => [test_app, only, stop],
                   description => <<"The handler raised an exception">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{kind => atom(), reason => atom(), stacktrace => term()}">>
                  }.

-telemetry_event #{
                  event => [test_app, cache, miss],
                  description => <<"Emitted at the start of the handler">>,
                  measurements => <<"#{system_time => non_neg_integer()}">>,
                  metadata => <<"#{}">>
                 }.
-telemetry_event #{
                   event => [test_app, cache, hit],
                   description => <<"Emitted at the end of the handler">>,
                   measurements => <<"#{duration => non_neg_integer()}">>,
                   metadata => <<"#{}">>
                  }.

handler(Args) ->
    _ = telemetry:span(
          [test_app, handler],
          #{},
          fun() ->
                  case Args of
                      raise_exception ->
                          binary_to_list("heh, already a list");
                      _ -> {nested_span(), #{}}
                  end
          end).

nested_span() ->
    _ = telemetry:span(
          [test_app, nested_span],
          #{},
          fun() ->
              {ok, #{}}
          end).
