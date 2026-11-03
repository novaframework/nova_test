-module(test_app_router).
-behaviour(nova_router).

-export([routes/1]).

routes(_Env) ->
    [
        #{
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
            plugins => [
                {pre_request, nova_request_plugin, #{parse_qs => true}}
            ],
            routes => [
                {"/test/query", fun test_controller:echo_query/1, #{methods => [get]}}
            ]
        },
        #{
            prefix => "",
            security => false,
            plugins => [
                {pre_request, nova_request_plugin, #{read_urlencoded_body => true}},
                {pre_request, nova_csrf_plugin, #{}}
            ],
            routes => [
                {"/test/form", fun test_controller:csrf_form/1, #{methods => [get]}},
                {"/test/form", fun test_controller:csrf_submit/1, #{methods => [post]}},
                {"/test/logout", fun test_controller:log_out/1, #{methods => [post]}}
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
                {"/test/ws", test_ws_handler, #{protocol => ws}},
                {"/test/login", fun test_controller:log_in/1, #{methods => [get]}},
                {"/test/ws-auth", test_ws_auth_handler, #{protocol => ws}}
            ]
        }
    ].
