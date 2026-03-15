-module(test_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Env) ->
    [#{
        prefix => "",
        security => false,
        plugins => [],
        routes => [
            {"/test/upload", fun test_controller:upload/1, #{methods => [post]}}
        ]
    },
    #{
        prefix => "",
        security => false,
        routes => [
            {"/test/json", fun test_controller:get_json/1, #{methods => [get]}},
            {"/test/json", fun test_controller:post_json/1, #{methods => [post]}},
            {"/test/status", fun test_controller:get_status/1, #{methods => [get]}},
            {"/test/redirect", fun test_controller:get_redirect/1, #{methods => [get]}},
            {"/test/echo/:id", fun test_controller:echo_id/1, #{methods => [get]}},
            {"/test/headers", fun test_controller:get_headers/1, #{methods => [get]}},
            {"/test/set-cookie", fun test_controller:set_cookie/1, #{methods => [get]}},
            {"/test/get-cookie", fun test_controller:get_cookie/1, #{methods => [get]}},
            {"/test/ws", test_ws_handler, #{protocol => ws}}
        ]
    }].
