-module(nova_test_start_SUITE).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    port_in_use_fails_test/1,
    bound_port_is_used_test/1,
    tls_listener_fails_test/1,
    stop_stops_what_start_started_test/1,
    app_stops_before_its_dependencies_test/1,
    inets_started_by_start_is_stopped_test/1,
    running_inets_survives_stop_test/1,
    failed_start_stops_inets_test/1,
    threaded_config_stops_both_test/1,
    already_running_apps_survive_stop_test/1,
    stop_in_start_order_test/1,
    stop_in_reverse_order_test/1,
    running_app_kept_when_listener_missing_test/1,
    running_app_kept_on_tls_test/1
]).

all() ->
    [
        port_in_use_fails_test,
        bound_port_is_used_test,
        tls_listener_fails_test,
        stop_stops_what_start_started_test,
        app_stops_before_its_dependencies_test,
        inets_started_by_start_is_stopped_test,
        running_inets_survives_stop_test,
        failed_start_stops_inets_test,
        threaded_config_stops_both_test,
        already_running_apps_survive_stop_test,
        stop_in_start_order_test,
        stop_in_reverse_order_test,
        running_app_kept_when_listener_missing_test,
        running_app_kept_on_tls_test
    ].

port_in_use_fails_test(Config) ->
    {Socket, Port} = take_port(),
    ok = test_app_env:load(Port),
    try
        ?assertError(
            {nova_test_listener_not_running, #{app := test_app, configured_port := Port}},
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
    ok = load_tls_env(),
    ?assertError(
        {nova_test_tls_not_supported, #{app := test_app, port := _}},
        nova_test:start(test_app, Config)
    ),
    ?assertNot(lists:keymember(nova, 1, application:which_applications())).

stop_stops_what_start_started_test(Config0) ->
    ok = test_app_env:load(0),
    ok = stop_inets(),
    Before = running(),
    Config = nova_test:start(test_app, Config0),
    Started = started(Config),
    NewlyRunning = running() -- Before,
    ?assertEqual(lists:sort(NewlyRunning), lists:sort(Started)),
    ?assertEqual(length(NewlyRunning), length(Started)),
    ?assertEqual(inets, hd(Started)),
    ?assertEqual(test_app, lists:last(Started)),
    ?assertEqual([], [A || A <- [nova, cowboy, ranch], not lists:member(A, Started)]),
    ok = nova_test:stop(Config),
    ?assertEqual(Before, running()).

app_stops_before_its_dependencies_test(Config0) ->
    ok = test_app_env:load(0),
    _ = persistent_term:erase({test_app, running_at_stop}),
    Config = nova_test:start(test_app, Config0),
    ok = nova_test:stop(Config),
    ?assertEqual(
        [nova_sup, cowboy_sup, ranch_sup],
        persistent_term:get({test_app, running_at_stop})
    ).

inets_started_by_start_is_stopped_test(Config0) ->
    ok = test_app_env:load(0),
    ok = stop_inets(),
    Config = nova_test:start(test_app, Config0),
    ?assert(lists:member(inets, started(Config))),
    ok = nova_test:stop(Config),
    ?assertNot(lists:member(inets, running())).

running_inets_survives_stop_test(Config0) ->
    ok = test_app_env:load(0),
    {ok, InetsStarted} = application:ensure_all_started(inets),
    try
        Config = nova_test:start(test_app, Config0),
        ?assertNot(lists:member(inets, started(Config))),
        ok = nova_test:stop(Config),
        ?assert(lists:member(inets, running()))
    after
        stop_all(InetsStarted)
    end.

failed_start_stops_inets_test(Config) ->
    ok = stop_inets(),
    ?assertError(
        {nova_test_start_failed, #{app := nova_test_no_such_app}},
        nova_test:start(nova_test_no_such_app, Config)
    ),
    ?assertNot(lists:member(inets, running())).

threaded_config_stops_both_test(Config0) ->
    ok = test_app_env:load(0),
    ok = load_library_app(test_app_b),
    Before = running(),
    ok = nova_test:stop(nova_test:start(test_app_b, nova_test:start(test_app, Config0))),
    ?assertEqual([], running() -- Before).

already_running_apps_survive_stop_test(Config0) ->
    ok = test_app_env:load(0),
    ok = load_library_app(test_app_b),
    {ok, InetsRunning} = application:ensure_all_started(inets),
    {ok, AppRunning} = application:ensure_all_started(test_app),
    Running = InetsRunning ++ AppRunning,
    try
        ConfigB = nova_test:start(test_app_b, Config0),
        ?assertEqual([test_app_b], proplists:get_value(nova_test_started, ConfigB)),
        ok = nova_test:stop(ConfigB),
        assert_stopped([test_app_b]),
        ?assert(lists:keymember(test_app, 1, application:which_applications())),
        ?assert(lists:keymember(nova, 1, application:which_applications())),
        {ok, Resp} = nova_test:get("/test/json", ConfigB),
        ?assertStatus(200, Resp)
    after
        stop_all(Running)
    end.

stop_in_start_order_test(Config0) ->
    ok = test_app_env:load(0),
    ok = load_library_app(test_app_b),
    ConfigA = nova_test:start(test_app, Config0),
    ConfigB = nova_test:start(test_app_b, Config0),
    ok = nova_test:stop(ConfigA),
    assert_stopped([test_app, nova, cowboy, ranch]),
    ok = nova_test:stop(ConfigB),
    assert_stopped([test_app, test_app_b, nova, cowboy, ranch]).

stop_in_reverse_order_test(Config0) ->
    ok = test_app_env:load(0),
    ok = load_library_app(test_app_b),
    ConfigA = nova_test:start(test_app, Config0),
    ConfigB = nova_test:start(test_app_b, Config0),
    ok = nova_test:stop(ConfigB),
    ?assert(lists:keymember(nova, 1, application:which_applications())),
    ok = nova_test:stop(ConfigA),
    assert_stopped([test_app, test_app_b, nova, cowboy, ranch]).

running_app_kept_when_listener_missing_test(Config) ->
    {Socket, Port} = take_port(),
    ok = test_app_env:load(Port),
    {ok, Started} = application:ensure_all_started(test_app),
    try
        ?assertError({nova_test_listener_not_running, _}, nova_test:start(test_app, Config)),
        ?assert(lists:keymember(test_app, 1, application:which_applications())),
        ?assert(lists:keymember(nova, 1, application:which_applications()))
    after
        stop_all(Started),
        gen_tcp:close(Socket)
    end.

running_app_kept_on_tls_test(Config) ->
    ok = load_tls_env(),
    {ok, Started} = application:ensure_all_started(test_app),
    try
        ?assertError({nova_test_tls_not_supported, _}, nova_test:start(test_app, Config)),
        ?assert(lists:keymember(test_app, 1, application:which_applications()))
    after
        stop_all(Started)
    end.

running() ->
    [A || {A, _, _} <- application:which_applications()].

started(Config) ->
    case proplists:get_value(nova_test_started, Config) of
        Apps when is_list(Apps) -> Apps
    end.

stop_inets() ->
    case application:stop(inets) of
        ok -> ok;
        {error, {not_started, inets}} -> ok
    end.

take_port() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Socket),
    {Socket, Port}.

assert_stopped(Apps) ->
    Running = [A || {A, _, _} <- application:which_applications()],
    ?assertEqual([], [A || A <- Apps, lists:member(A, Running)]).

load_tls_env() ->
    {ok, _} = application:ensure_all_started(ssl),
    #{server_config := Server} = public_key:pkix_test_data(#{
        server_chain => #{root => [], intermediates => [], peer => []},
        client_chain => #{root => [], intermediates => [], peer => []}
    }),
    SslOptions = maps:with([cert, key], maps:from_list(Server)),
    test_app_env:load(0, #{use_ssl => true, ssl_port => 0, ssl_options => SslOptions}).

-spec stop_all([atom()]) -> ok.
stop_all(Apps) ->
    _ = [application:stop(A) || A <- lists:reverse(Apps), is_atom(A)],
    ok.

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
