-module(nova_test_SUITE).
-include_lib("nova_test/include/nova_test.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%% Integration tests
-export([
    get_json_test/1,
    post_json_test/1,
    get_status_test/1,
    get_redirect_test/1,
    get_echo_id_test/1,
    get_headers_test/1,
    status_accessor_test/1,
    body_accessor_test/1,
    json_accessor_test/1,
    header_accessor_test/1,
    delete_request_test/1
]).

%% Unit tests (request builder)
-export([
    req_new_test/1,
    req_with_bindings_test/1,
    req_with_json_test/1,
    req_with_header_test/1,
    req_with_auth_data_test/1,
    req_with_query_test/1,
    req_with_body_test/1,
    req_with_peer_test/1,
    req_new_string_path_test/1,
    req_with_multipart_test/1,
    req_with_cookies_test/1
]).

%% Controller assertion tests
-export([
    assert_json_response_test/1,
    assert_json_response_with_status_test/1,
    assert_status_response_test/1,
    assert_redirect_test/1,
    assert_ok_response_test/1
]).

%% Direct controller call tests
-export([
    direct_controller_call_test/1
]).

%% Cookie tests
-export([
    save_cookies_test/1,
    set_cookie_test/1,
    clear_cookies_test/1,
    cookie_round_trip_test/1
]).

%% Multipart tests
-export([
    multipart_field_test/1,
    multipart_file_test/1,
    multipart_mixed_test/1
]).

%% WebSocket tests
-export([
    ws_connect_close_test/1,
    ws_echo_test/1,
    ws_json_test/1,
    ws_recv_timeout_test/1
]).

%% Logging tests
-export([
    logging_test/1
]).

all() ->
    [
        {group, integration},
        {group, unit},
        {group, assertions},
        {group, direct},
        {group, cookies},
        {group, multipart},
        {group, websocket},
        {group, logging}
    ].

groups() ->
    [
        {integration, [sequence], [
            get_json_test,
            post_json_test,
            get_status_test,
            get_redirect_test,
            get_echo_id_test,
            get_headers_test,
            status_accessor_test,
            body_accessor_test,
            json_accessor_test,
            header_accessor_test,
            delete_request_test
        ]},
        {unit, [parallel], [
            req_new_test,
            req_with_bindings_test,
            req_with_json_test,
            req_with_header_test,
            req_with_auth_data_test,
            req_with_query_test,
            req_with_body_test,
            req_with_peer_test,
            req_new_string_path_test,
            req_with_multipart_test,
            req_with_cookies_test
        ]},
        {assertions, [parallel], [
            assert_json_response_test,
            assert_json_response_with_status_test,
            assert_status_response_test,
            assert_redirect_test,
            assert_ok_response_test
        ]},
        {direct, [parallel], [
            direct_controller_call_test
        ]},
        {cookies, [sequence], [
            save_cookies_test,
            set_cookie_test,
            clear_cookies_test,
            cookie_round_trip_test
        ]},
        {multipart, [sequence], [
            multipart_field_test,
            multipart_file_test,
            multipart_mixed_test
        ]},
        {websocket, [sequence], [
            ws_connect_close_test,
            ws_echo_test,
            ws_json_test,
            ws_recv_timeout_test
        ]},
        {logging, [sequence], [
            logging_test
        ]}
    ].

init_per_suite(Config) ->
    ok = application:load(
        {application, test_app, [
            {description, "Test app for nova_test"},
            {vsn, "0.0.1"},
            {mod, {test_app, []}},
            {applications, [kernel, stdlib, nova]},
            {modules, [test_app, test_sup, test_app_router, test_controller, test_ws_handler]}
        ]}
    ),
    application:set_env(nova, bootstrap_application, test_app),
    application:set_env(nova, cowboy_configuration, #{port => 48484}),
    application:set_env(nova, plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]),
    nova_test:start(test_app, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%% Integration tests

get_json_test(Config) ->
    {ok, Resp} = nova_test:get("/test/json", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"ok">> := true}, Resp).

post_json_test(Config) ->
    {ok, Resp} = nova_test:post(
        "/test/json",
        #{json => #{<<"name">> => <<"Alice">>}},
        Config
    ),
    ?assertStatus(201, Resp),
    ?assertJson(#{<<"name">> := <<"Alice">>}, Resp).

get_status_test(Config) ->
    {ok, Resp} = nova_test:get("/test/status", Config),
    ?assertStatus(204, Resp).

get_redirect_test(Config) ->
    {ok, Resp} = nova_test:get("/test/redirect", Config),
    ?assertStatus(302, Resp),
    Location = nova_test:header("location", Resp),
    ?assertNotEqual(undefined, Location).

get_echo_id_test(Config) ->
    {ok, Resp} = nova_test:get("/test/echo/42", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"id">> := <<"42">>}, Resp).

get_headers_test(Config) ->
    {ok, Resp} = nova_test:get("/test/headers", Config),
    ?assertStatus(200, Resp),
    Value = nova_test:header("x-custom", Resp),
    ?assertEqual("test-value", Value).

status_accessor_test(Config) ->
    {ok, Resp} = nova_test:get("/test/json", Config),
    ?assertEqual(200, nova_test:status(Resp)).

body_accessor_test(Config) ->
    {ok, Resp} = nova_test:get("/test/json", Config),
    Body = nova_test:body(Resp),
    ?assert(is_binary(Body)),
    ?assert(byte_size(Body) > 0).

json_accessor_test(Config) ->
    {ok, Resp} = nova_test:get("/test/json", Config),
    Json = nova_test:json(Resp),
    ?assert(is_map(Json)),
    ?assertEqual(true, maps:get(<<"ok">>, Json)).

header_accessor_test(Config) ->
    {ok, Resp} = nova_test:get("/test/json", Config),
    CT = nova_test:header("content-type", Resp),
    ?assertNotEqual(undefined, CT).

delete_request_test(Config) ->
    {ok, Resp} = nova_test:delete("/test/nonexistent", Config),
    Status = nova_test:status(Resp),
    ?assert(Status >= 400).

%% Unit tests (request builder)

req_new_test(_Config) ->
    Req = nova_test_req:new(get, <<"/users">>),
    ?assertEqual(<<"GET">>, maps:get(method, Req)),
    ?assertEqual(<<"/users">>, maps:get(path, Req)),
    ?assertEqual(<<"localhost">>, maps:get(host, Req)),
    ?assertEqual(8080, maps:get(port, Req)),
    ?assertEqual(<<"http">>, maps:get(scheme, Req)),
    ?assertEqual(<<>>, maps:get(qs, Req)),
    ?assertEqual('HTTP/1.1', maps:get(version, Req)),
    ?assertEqual(#{}, maps:get(headers, Req)),
    ?assertEqual(#{}, maps:get(bindings, Req)),
    ?assertMatch({{127, 0, 0, 1}, _}, maps:get(peer, Req)).

req_with_bindings_test(_Config) ->
    Req = nova_test_req:new(get, <<"/users/1">>),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"1">>}, Req),
    ?assertEqual(#{<<"id">> => <<"1">>}, maps:get(bindings, Req1)).

req_with_json_test(_Config) ->
    Req = nova_test_req:new(post, <<"/users">>),
    Req1 = nova_test_req:with_json(#{<<"name">> => <<"Alice">>}, Req),
    ?assertEqual(#{<<"name">> => <<"Alice">>}, maps:get(json, Req1)),
    ?assert(maps:is_key(body, Req1)),
    Headers = maps:get(headers, Req1),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)).

req_with_header_test(_Config) ->
    Req = nova_test_req:new(get, <<"/users">>),
    Req1 = nova_test_req:with_header(<<"authorization">>, <<"Bearer token123">>, Req),
    Headers = maps:get(headers, Req1),
    ?assertEqual(<<"Bearer token123">>, maps:get(<<"authorization">>, Headers)).

req_with_auth_data_test(_Config) ->
    Req = nova_test_req:new(get, <<"/admin">>),
    Req1 = nova_test_req:with_auth_data(#{role => admin}, Req),
    ?assertEqual(#{role => admin}, maps:get(auth_data, Req1)).

req_with_query_test(_Config) ->
    Req = nova_test_req:new(get, <<"/search">>),
    Req1 = nova_test_req:with_query(#{<<"q">> => <<"erlang">>, <<"page">> => <<"1">>}, Req),
    QS = maps:get(qs, Req1),
    ?assert(is_binary(QS)),
    ?assert(binary:match(QS, <<"q=erlang">>) =/= nomatch),
    ?assert(binary:match(QS, <<"page=1">>) =/= nomatch).

req_with_body_test(_Config) ->
    Req = nova_test_req:new(post, <<"/upload">>),
    Req1 = nova_test_req:with_body(<<"raw data">>, Req),
    ?assertEqual(<<"raw data">>, maps:get(body, Req1)).

req_with_peer_test(_Config) ->
    Req = nova_test_req:new(get, <<"/info">>),
    Peer = {{192, 168, 1, 1}, 54321},
    Req1 = nova_test_req:with_peer(Peer, Req),
    ?assertEqual(Peer, maps:get(peer, Req1)).

req_new_string_path_test(_Config) ->
    Req = nova_test_req:new(post, "/users"),
    ?assertEqual(<<"POST">>, maps:get(method, Req)),
    ?assertEqual(<<"/users">>, maps:get(path, Req)).

req_with_multipart_test(_Config) ->
    Req = nova_test_req:new(post, <<"/upload">>),
    Fields = [{field, <<"name">>, <<"test">>}],
    Req1 = nova_test_req:with_multipart(Fields, Req),
    ?assert(maps:is_key(body, Req1)),
    ?assertEqual(Fields, maps:get(multipart, Req1)),
    Headers = maps:get(headers, Req1),
    CT = maps:get(<<"content-type">>, Headers),
    ?assertMatch(<<"multipart/form-data; boundary=", _/binary>>, CT).

req_with_cookies_test(_Config) ->
    Req = nova_test_req:new(get, <<"/protected">>),
    Req1 = nova_test_req:with_cookies(#{<<"session">> => <<"abc">>}, Req),
    Headers = maps:get(headers, Req1),
    CookieHeader = maps:get(<<"cookie">>, Headers),
    ?assertEqual(<<"session=abc">>, CookieHeader).

%% Controller assertion tests

assert_json_response_test(_Config) ->
    Result = {json, #{<<"name">> => <<"Alice">>}},
    ?assertJsonResponse(#{<<"name">> := <<"Alice">>}, Result).

assert_json_response_with_status_test(_Config) ->
    Result = {json, 201, #{}, #{<<"id">> => 1}},
    ?assertJsonResponse(201, #{<<"id">> := 1}, Result).

assert_status_response_test(_Config) ->
    Result = {status, 204},
    ?assertStatusResponse(204, Result).

assert_redirect_test(_Config) ->
    Result = {redirect, <<"/login">>},
    ?assertRedirect(<<"/login">>, Result).

assert_ok_response_test(_Config) ->
    Result = {ok, #{name => <<"Alice">>}},
    ?assertOkResponse(#{name := <<"Alice">>}, Result).

%% Direct controller call tests

direct_controller_call_test(_Config) ->
    Req = nova_test_req:new(get, <<"/test/echo/99">>),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"99">>}, Req),
    Result = test_controller:echo_id(Req1),
    ?assertJsonResponse(#{<<"id">> := <<"99">>}, Result).

%% Cookie tests

save_cookies_test(_Config) ->
    Resp = #{
        status => 200,
        headers => [{"set-cookie", "session=abc123; Path=/; HttpOnly"}],
        body => <<"{}">>
    },
    Config = nova_test:save_cookies(Resp, []),
    ?assertEqual(<<"abc123">>, nova_test:cookie(<<"session">>, Config)).

set_cookie_test(_Config) ->
    Config = nova_test:set_cookie(<<"token">>, <<"xyz">>, []),
    ?assertEqual(<<"xyz">>, nova_test:cookie(<<"token">>, Config)),
    Config2 = nova_test:set_cookie(<<"token">>, <<"new">>, Config),
    ?assertEqual(<<"new">>, nova_test:cookie(<<"token">>, Config2)).

clear_cookies_test(_Config) ->
    Config = nova_test:set_cookie(<<"a">>, <<"1">>, []),
    Config2 = nova_test:clear_cookies(Config),
    ?assertEqual(#{}, nova_test:cookies(Config2)).

cookie_round_trip_test(Config) ->
    {ok, Resp} = nova_test:get("/test/set-cookie", Config),
    ?assertStatus(200, Resp),
    Config2 = nova_test:save_cookies(Resp, Config),
    ?assertEqual(<<"abc123">>, nova_test:cookie(<<"test_session">>, Config2)),
    {ok, Resp2} = nova_test:get("/test/get-cookie", Config2),
    ?assertStatus(200, Resp2),
    ?assertJson(#{<<"test_session">> := <<"abc123">>}, Resp2).

%% Multipart tests

multipart_field_test(Config) ->
    {ok, Resp} = nova_test:post(
        "/test/upload",
        #{multipart => [{field, <<"name">>, <<"Alice">>}]},
        Config
    ),
    ?assertStatus(201, Resp),
    Json = nova_test:json(Resp),
    [Part] = maps:get(<<"parts">>, Json),
    ?assertEqual(<<"name">>, maps:get(<<"name">>, Part)),
    ?assertEqual(<<"Alice">>, maps:get(<<"body">>, Part)).

multipart_file_test(Config) ->
    FileData = <<"hello world">>,
    {ok, Resp} = nova_test:post(
        "/test/upload",
        #{multipart => [{file, <<"doc">>, <<"test.txt">>, <<"text/plain">>, FileData}]},
        Config
    ),
    ?assertStatus(201, Resp),
    Json = nova_test:json(Resp),
    [Part] = maps:get(<<"parts">>, Json),
    ?assertEqual(<<"doc">>, maps:get(<<"name">>, Part)),
    ?assertEqual(<<"test.txt">>, maps:get(<<"filename">>, Part)),
    ?assertEqual(byte_size(FileData), maps:get(<<"size">>, Part)).

multipart_mixed_test(Config) ->
    {ok, Resp} = nova_test:post(
        "/test/upload",
        #{
            multipart => [
                {field, <<"title">>, <<"My Doc">>},
                {file, <<"file">>, <<"doc.pdf">>, <<"application/pdf">>, <<"pdf-data">>}
            ]
        },
        Config
    ),
    ?assertStatus(201, Resp),
    Json = nova_test:json(Resp),
    Parts = maps:get(<<"parts">>, Json),
    ?assertEqual(2, length(Parts)).

%% WebSocket tests

ws_connect_close_test(Config) ->
    {ok, Conn} = nova_test_ws:connect("/test/ws", Config),
    ok = nova_test_ws:close(Conn).

ws_echo_test(Config) ->
    {ok, Conn} = nova_test_ws:connect("/test/ws", Config),
    ok = nova_test_ws:send_text(<<"hello">>, Conn),
    ?assertWsRecv(<<"hello">>, Conn),
    ok = nova_test_ws:close(Conn).

ws_json_test(Config) ->
    {ok, Conn} = nova_test_ws:connect("/test/ws", Config),
    Msg = #{<<"action">> => <<"ping">>},
    ok = nova_test_ws:send_json(Msg, Conn),
    {ok, Received} = nova_test_ws:recv_json(Conn),
    ?assertEqual(<<"ping">>, maps:get(<<"action">>, Received)),
    ok = nova_test_ws:close(Conn).

ws_recv_timeout_test(Config) ->
    {ok, Conn} = nova_test_ws:connect("/test/ws", Config),
    ?assertEqual({error, timeout}, nova_test_ws:recv(Conn, 100)),
    ok = nova_test_ws:close(Conn).

%% Logging tests

logging_test(Config) ->
    Config2 = nova_test:enable_logging(Config),
    {ok, Resp} = nova_test:get("/test/json", Config2),
    ?assertStatus(200, Resp),
    Config3 = nova_test:disable_logging(Config2),
    {ok, Resp2} = nova_test:get("/test/json", Config3),
    ?assertStatus(200, Resp2).
