-module(nova_test_hrl_tests).
-include_lib("nova_test/include/nova_test.hrl").

response() ->
    #{status => 200, headers => [{"x-id", "7"}], body => ~"{\"ok\":true}"}.

http_assertions_test_() ->
    Resp = response(),
    [
        ?_assertStatus(200, Resp),
        ?_assertJson(#{~"ok" := true}, Resp),
        ?_assertBody(~"{\"ok\":true}", Resp),
        ?_assertHeader("x-id", "7", Resp),
        ?_assertCookie(~"sid", ~"abc", nova_test:set_cookie(~"sid", ~"abc", []))
    ].

controller_assertions_test_() ->
    [
        ?_assertJsonResponse(#{~"id" := 1}, {json, #{~"id" => 1}}),
        ?_assertJsonResponse(201, #{~"id" := 1}, {json, 201, #{}, #{~"id" => 1}}),
        ?_assertStatusResponse(204, {status, 204}),
        ?_assertRedirect(~"/login", {redirect, ~"/login"}),
        ?_assertOkResponse(#{name := _}, {ok, #{name => ~"alice"}})
    ].

generators_are_lazy_test() ->
    Test = ?_assertStatus(500, response()),
    ?assertMatch({_, Fun} when is_function(Fun, 0), Test),
    {_, Fun} = Test,
    ?assertError({assertEqual, _}, Fun()).
