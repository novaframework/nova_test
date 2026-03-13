-module(nova_test_response_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for nova_test response accessor functions

status_test() ->
    Resp = #{status => 200, headers => [], body => <<>>},
    ?assertEqual(200, nova_test:status(Resp)).

status_404_test() ->
    Resp = #{status => 404, headers => [], body => <<>>},
    ?assertEqual(404, nova_test:status(Resp)).

body_test() ->
    Resp = #{status => 200, headers => [], body => <<"hello">>},
    ?assertEqual(<<"hello">>, nova_test:body(Resp)).

body_empty_test() ->
    Resp = #{status => 204, headers => [], body => <<>>},
    ?assertEqual(<<>>, nova_test:body(Resp)).

json_decode_test() ->
    Resp = #{status => 200, headers => [], body => <<"{\"ok\":true}">>},
    ?assertEqual(#{<<"ok">> => true}, nova_test:json(Resp)).

json_nested_test() ->
    Resp = #{status => 200, headers => [], body => <<"{\"user\":{\"name\":\"Alice\"}}">>},
    ?assertMatch(#{<<"user">> := #{<<"name">> := <<"Alice">>}}, nova_test:json(Resp)).

headers_test() ->
    Hdrs = [{"content-type", "application/json"}],
    Resp = #{status => 200, headers => Hdrs, body => <<>>},
    ?assertEqual(Hdrs, nova_test:headers(Resp)).

header_found_test() ->
    Resp = #{status => 200, headers => [{"Content-Type", "application/json"}], body => <<>>},
    ?assertEqual("application/json", nova_test:header("content-type", Resp)).

header_case_insensitive_test() ->
    Resp = #{status => 200, headers => [{"X-Custom", "value"}], body => <<>>},
    ?assertEqual("value", nova_test:header("x-custom", Resp)),
    ?assertEqual("value", nova_test:header("X-CUSTOM", Resp)),
    ?assertEqual("value", nova_test:header("X-Custom", Resp)).

header_not_found_test() ->
    Resp = #{status => 200, headers => [], body => <<>>},
    ?assertEqual(undefined, nova_test:header("x-missing", Resp)).

header_first_match_test() ->
    Resp = #{status => 200, headers => [{"X-Val", "first"}, {"X-Val", "second"}], body => <<>>},
    ?assertEqual("first", nova_test:header("x-val", Resp)).
