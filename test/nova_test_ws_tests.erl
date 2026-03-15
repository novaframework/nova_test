-module(nova_test_ws_tests).
-include_lib("eunit/include/eunit.hrl").

maybe_log_ws_disabled_test() ->
    Config = [{nova_test_logging, false}],
    ?assertEqual(ok, nova_test_ws:maybe_log_ws("TEST", "detail", Config)).

maybe_log_ws_default_test() ->
    ?assertEqual(ok, nova_test_ws:maybe_log_ws("TEST", "detail", [])).
