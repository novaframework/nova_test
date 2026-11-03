-module(nova_test_start_SUITE).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    port_in_use_fails_test/1,
    bound_port_is_used_test/1
]).

-define(TAKEN_PORT, 58610).

all() ->
    [port_in_use_fails_test, bound_port_is_used_test].

port_in_use_fails_test(Config) ->
    ok = test_app_env:load(?TAKEN_PORT),
    {ok, Socket} = gen_tcp:listen(?TAKEN_PORT, [{reuseaddr, true}]),
    try
        ?assertError(
            {nova_test_listener_not_running, #{app := test_app, configured_port := ?TAKEN_PORT}},
            nova_test:start(test_app, Config)
        ),
        ?assertNot(lists:keymember(test_app, 1, application:which_applications())),
        ?assertNot(lists:keymember(nova, 1, application:which_applications()))
    after
        gen_tcp:close(Socket)
    end.

bound_port_is_used_test(Config0) ->
    ok = test_app_env:load(0),
    Config = nova_test:start(test_app, Config0),
    try
        Port = proplists:get_value(nova_test_port, Config),
        ?assert(is_integer(Port) andalso Port > 0),
        {ok, Resp} = nova_test:get("/test/json", Config),
        ?assertStatus(200, Resp)
    after
        nova_test:stop(Config)
    end.
