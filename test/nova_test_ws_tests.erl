-module(nova_test_ws_tests).
-include_lib("eunit/include/eunit.hrl").

maybe_log_ws_disabled_test() ->
    Config = [{nova_test_logging, false}],
    ?assertEqual(ok, nova_test_ws:maybe_log_ws("TEST", "detail", Config)).

maybe_log_ws_default_test() ->
    ?assertEqual(ok, nova_test_ws:maybe_log_ws("TEST", "detail", [])).

connect_invalid_path_test_() ->
    Config = [{nova_test_port, 1}],
    [
        ?_assertError(
            {nova_test_invalid_path, #{path := "@127.0.0.1:1/x"}},
            nova_test_ws:connect("@127.0.0.1:1/x", Config)
        ),
        ?_assertError({nova_test_invalid_path, #{path := "x"}}, nova_test_ws:connect("x", Config))
    ].
