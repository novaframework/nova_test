-module(nova_test_start_SUITE).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    port_in_use_fails_test/1,
    bound_port_is_used_test/1,
    tls_listener_fails_test/1,
    second_app_keeps_listener_test/1
]).

-define(TAKEN_PORT, 58610).

all() ->
    [
        port_in_use_fails_test,
        bound_port_is_used_test,
        tls_listener_fails_test,
        second_app_keeps_listener_test
    ].

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

tls_listener_fails_test(Config) ->
    {ok, _} = application:ensure_all_started(ssl),
    #{server_config := Server} = public_key:pkix_test_data(#{
        server_chain => #{root => [], intermediates => [], peer => []},
        client_chain => #{root => [], intermediates => [], peer => []}
    }),
    SslOptions = maps:with([cert, key], maps:from_list(Server)),
    ok = test_app_env:load(0, #{use_ssl => true, ssl_port => 0, ssl_options => SslOptions}),
    ?assertError(
        {nova_test_tls_not_supported, #{app := test_app, port := _}},
        nova_test:start(test_app, Config)
    ),
    ?assertNot(lists:keymember(nova, 1, application:which_applications())).

second_app_keeps_listener_test(Config0) ->
    ok = test_app_env:load(0),
    ok = load_library_app(test_app_b),
    ConfigA = nova_test:start(test_app, Config0),
    ConfigB = nova_test:start(test_app_b, Config0),
    try
        ?assertEqual(
            proplists:get_value(nova_test_port, ConfigA),
            proplists:get_value(nova_test_port, ConfigB)
        ),
        ok = nova_test:stop(ConfigA),
        ?assert(lists:keymember(nova, 1, application:which_applications())),
        {ok, Resp} = nova_test:get("/test/json", ConfigB),
        ?assertStatus(200, Resp),
        ok = nova_test:stop(ConfigB),
        ?assert(lists:keymember(nova, 1, application:which_applications()))
    after
        nova_test:stop(ConfigB),
        nova_test:stop(ConfigA)
    end,
    ?assertNot(lists:keymember(nova, 1, application:which_applications())).

load_library_app(App) ->
    case
        application:load(
            {application, App, [
                {description, "Second app on the bootstrap listener"},
                {vsn, "0.0.1"},
                {applications, [kernel, stdlib, nova]},
                {modules, []}
            ]}
        )
    of
        ok -> ok;
        {error, {already_loaded, _}} -> ok
    end.
