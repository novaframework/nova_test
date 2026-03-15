-module(nova_test_tests).
-include_lib("eunit/include/eunit.hrl").

%% Response accessors

status_test() ->
    ?assertEqual(200, nova_test:status(#{status => 200, headers => [], body => <<>>})).

status_404_test() ->
    ?assertEqual(404, nova_test:status(#{status => 404, headers => [], body => <<>>})).

body_test() ->
    ?assertEqual(<<"hello">>, nova_test:body(#{status => 200, headers => [], body => <<"hello">>})).

body_empty_test() ->
    ?assertEqual(<<>>, nova_test:body(#{status => 200, headers => [], body => <<>>})).

json_test() ->
    Body = <<"{\"ok\":true}">>,
    Result = nova_test:json(#{status => 200, headers => [], body => Body}),
    ?assertEqual(true, maps:get(<<"ok">>, Result)).

json_nested_test() ->
    Body = <<"{\"user\":{\"name\":\"alice\"}}">>,
    Result = nova_test:json(#{status => 200, headers => [], body => Body}),
    ?assertMatch(#{<<"user">> := #{<<"name">> := <<"alice">>}}, Result).

headers_test() ->
    Hdrs = [{"content-type", "application/json"}],
    ?assertEqual(Hdrs, nova_test:headers(#{status => 200, headers => Hdrs, body => <<>>})).

header_found_test() ->
    Resp = #{status => 200, headers => [{"Content-Type", "text/html"}], body => <<>>},
    ?assertEqual("text/html", nova_test:header("content-type", Resp)).

header_missing_test() ->
    Resp = #{status => 200, headers => [], body => <<>>},
    ?assertEqual(undefined, nova_test:header("x-missing", Resp)).

header_case_insensitive_test() ->
    Resp = #{status => 200, headers => [{"X-Custom", "val"}], body => <<>>},
    ?assertEqual("val", nova_test:header("x-custom", Resp)).

%% Cookie management

save_cookies_test() ->
    Resp = #{status => 200, headers => [{"set-cookie", "sid=abc; Path=/"}], body => <<>>},
    Config = nova_test:save_cookies(Resp, []),
    ?assertEqual(<<"abc">>, nova_test:cookie(<<"sid">>, Config)).

save_cookies_merge_test() ->
    Config0 = nova_test:set_cookie(<<"a">>, <<"1">>, []),
    Resp = #{status => 200, headers => [{"set-cookie", "b=2"}], body => <<>>},
    Config1 = nova_test:save_cookies(Resp, Config0),
    ?assertEqual(<<"1">>, nova_test:cookie(<<"a">>, Config1)),
    ?assertEqual(<<"2">>, nova_test:cookie(<<"b">>, Config1)).

save_cookies_multiple_test() ->
    Resp = #{
        status => 200,
        headers => [{"set-cookie", "a=1"}, {"set-cookie", "b=2"}],
        body => <<>>
    },
    Config = nova_test:save_cookies(Resp, []),
    ?assertEqual(<<"1">>, nova_test:cookie(<<"a">>, Config)),
    ?assertEqual(<<"2">>, nova_test:cookie(<<"b">>, Config)).

cookies_empty_test() ->
    ?assertEqual(#{}, nova_test:cookies([])).

cookies_with_data_test() ->
    Config = nova_test:set_cookie(<<"k">>, <<"v">>, []),
    ?assertEqual(#{<<"k">> => <<"v">>}, nova_test:cookies(Config)).

cookie_found_test() ->
    Config = nova_test:set_cookie(<<"tok">>, <<"xyz">>, []),
    ?assertEqual(<<"xyz">>, nova_test:cookie(<<"tok">>, Config)).

cookie_missing_test() ->
    ?assertEqual(undefined, nova_test:cookie(<<"nope">>, [])).

set_cookie_new_test() ->
    Config = nova_test:set_cookie(<<"a">>, <<"1">>, []),
    ?assertEqual(<<"1">>, nova_test:cookie(<<"a">>, Config)).

set_cookie_overwrite_test() ->
    C1 = nova_test:set_cookie(<<"a">>, <<"1">>, []),
    C2 = nova_test:set_cookie(<<"a">>, <<"2">>, C1),
    ?assertEqual(<<"2">>, nova_test:cookie(<<"a">>, C2)).

clear_cookies_test() ->
    C1 = nova_test:set_cookie(<<"a">>, <<"1">>, []),
    C2 = nova_test:clear_cookies(C1),
    ?assertEqual(#{}, nova_test:cookies(C2)).

%% Logging

enable_logging_test() ->
    Config = nova_test:enable_logging([]),
    ?assertEqual(true, proplists:get_value(nova_test_logging, Config)).

disable_logging_test() ->
    Config = nova_test:enable_logging([]),
    Config2 = nova_test:disable_logging(Config),
    ?assertEqual(false, proplists:get_value(nova_test_logging, Config2)).

enable_idempotent_test() ->
    C1 = nova_test:enable_logging([]),
    C2 = nova_test:enable_logging(C1),
    ?assertEqual(true, proplists:get_value(nova_test_logging, C2)),
    ?assertEqual(1, length([K || {K, _} <- C2, K =:= nova_test_logging])).

%% Internal: build_headers

build_headers_with_test() ->
    Result = nova_test:build_headers(#{headers => [{<<"accept">>, <<"text/html">>}]}),
    ?assertEqual([{"accept", "text/html"}], Result).

build_headers_empty_test() ->
    ?assertEqual([], nova_test:build_headers(#{})).

build_headers_no_key_test() ->
    ?assertEqual([], nova_test:build_headers(#{json => #{}})).

%% Internal: build_request

build_request_plain_test() ->
    Result = nova_test:build_request("http://localhost/x", [], #{}),
    ?assertEqual({"http://localhost/x", []}, Result).

build_request_json_test() ->
    Result = nova_test:build_request("http://localhost/x", [], #{json => #{<<"k">> => <<"v">>}}),
    ?assertMatch({"http://localhost/x", [], "application/json", _}, Result).

build_request_multipart_test() ->
    Fields = [{field, <<"name">>, <<"val">>}],
    Result = nova_test:build_request("http://localhost/x", [], #{multipart => Fields}),
    ?assertMatch({"http://localhost/x", [], "multipart/form-data; boundary=" ++ _, _}, Result).

build_request_body_ct_test() ->
    Result = nova_test:build_request("http://localhost/x", [], #{
        body => <<"x">>, content_type => "text/xml"
    }),
    ?assertMatch({"http://localhost/x", [], "text/xml", _}, Result).

build_request_body_default_ct_test() ->
    Result = nova_test:build_request("http://localhost/x", [], #{body => <<"x">>}),
    ?assertMatch({"http://localhost/x", [], "text/plain", _}, Result).

%% Internal: cookie_header

cookie_header_empty_test() ->
    ?assertEqual([], nova_test:cookie_header([])).

cookie_header_with_cookies_test() ->
    Config = nova_test:set_cookie(<<"session">>, <<"abc">>, []),
    [{"cookie", CookieStr}] = nova_test:cookie_header(Config),
    ?assert(is_list(CookieStr)),
    ?assertNotEqual("", CookieStr).

cookie_header_format_test() ->
    Config = [{nova_test_cookies, #{<<"a">> => <<"1">>}}],
    [{"cookie", CookieStr}] = nova_test:cookie_header(Config),
    ?assertEqual("a=1", CookieStr).

%% Internal: extract_set_cookies

extract_set_cookies_empty_test() ->
    ?assertEqual(#{}, nova_test:extract_set_cookies([])).

extract_set_cookies_single_test() ->
    Headers = [{"set-cookie", "name=val; Path=/"}],
    Result = nova_test:extract_set_cookies(Headers),
    ?assertEqual(#{<<"name">> => <<"val">>}, Result).

extract_set_cookies_non_cookie_test() ->
    Headers = [{"content-type", "text/html"}],
    ?assertEqual(#{}, nova_test:extract_set_cookies(Headers)).

extract_set_cookies_multiple_test() ->
    Headers = [{"set-cookie", "a=1"}, {"set-cookie", "b=2"}],
    Result = nova_test:extract_set_cookies(Headers),
    ?assertEqual(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}, Result).

extract_set_cookies_case_insensitive_test() ->
    Headers = [{"Set-Cookie", "x=y"}],
    Result = nova_test:extract_set_cookies(Headers),
    ?assertEqual(#{<<"x">> => <<"y">>}, Result).

extract_set_cookies_malformed_test() ->
    Headers = [{"set-cookie", "noequals"}, {"set-cookie", "good=val"}],
    Result = nova_test:extract_set_cookies(Headers),
    ?assertEqual(#{<<"good">> => <<"val">>}, Result).

%% Internal: parse_cookie

parse_cookie_simple_test() ->
    ?assertEqual({<<"name">>, <<"val">>}, nova_test:parse_cookie("name=val")).

parse_cookie_with_attrs_test() ->
    ?assertEqual({<<"sid">>, <<"abc">>}, nova_test:parse_cookie("sid=abc; Path=/; HttpOnly")).

parse_cookie_malformed_test() ->
    ?assertEqual(error, nova_test:parse_cookie("noequals")).

%% Internal: generate_boundary

generate_boundary_prefix_test() ->
    B = nova_test:generate_boundary(),
    ?assertMatch(<<"----nova_test_", _/binary>>, B).

generate_boundary_unique_test() ->
    B1 = nova_test:generate_boundary(),
    B2 = nova_test:generate_boundary(),
    ?assertNotEqual(B1, B2).

%% Internal: multipart_part

multipart_field_part_test() ->
    Boundary = <<"boundary">>,
    IOList = nova_test:multipart_part({field, <<"name">>, <<"alice">>}, Boundary),
    Bin = iolist_to_binary(IOList),
    ?assert(binary:match(Bin, <<"--boundary">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"name=\"name\"">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"alice">>) =/= nomatch).

multipart_file_part_test() ->
    Boundary = <<"boundary">>,
    IOList = nova_test:multipart_part(
        {file, <<"doc">>, <<"test.txt">>, <<"text/plain">>, <<"hello">>}, Boundary
    ),
    Bin = iolist_to_binary(IOList),
    ?assert(binary:match(Bin, <<"--boundary">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"name=\"doc\"">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"filename=\"test.txt\"">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"Content-Type: text/plain">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"hello">>) =/= nomatch).

%% Internal: maybe_log

maybe_log_disabled_test() ->
    Resp = #{status => 200, headers => [], body => <<>>},
    ?assertEqual(ok, nova_test:maybe_log(get, "/test", #{}, Resp, [])).

maybe_log_disabled_explicit_test() ->
    Resp = #{status => 200, headers => [], body => <<>>},
    Config = [{nova_test_logging, false}],
    ?assertEqual(ok, nova_test:maybe_log(get, "/test", #{}, Resp, Config)).

maybe_log_enabled_test() ->
    Resp = #{status => 200, headers => [], body => <<"ok">>},
    Config = [{nova_test_logging, true}],
    ?assertEqual(ok, nova_test:maybe_log(get, "/test", #{}, Resp, Config)).

maybe_log_enabled_with_json_test() ->
    Resp = #{status => 201, headers => [], body => <<"{}">>},
    Config = [{nova_test_logging, true}],
    ?assertEqual(
        ok, nova_test:maybe_log(post, "/api", #{json => #{<<"k">> => <<"v">>}}, Resp, Config)
    ).

maybe_log_enabled_with_body_test() ->
    Resp = #{status => 200, headers => [], body => <<"response">>},
    Config = [{nova_test_logging, true}],
    ?assertEqual(ok, nova_test:maybe_log(put, "/x", #{body => <<"req">>}, Resp, Config)).

%% build_multipart_body (already exported)

build_multipart_body_closing_test() ->
    Bin = iolist_to_binary(nova_test:build_multipart_body([], <<"bnd">>)),
    ?assert(binary:match(Bin, <<"--bnd--">>) =/= nomatch).

build_multipart_body_field_test() ->
    Fields = [{field, <<"k">>, <<"v">>}],
    Bin = iolist_to_binary(nova_test:build_multipart_body(Fields, <<"bnd">>)),
    ?assert(binary:match(Bin, <<"--bnd\r\n">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"v">>) =/= nomatch),
    ?assert(binary:match(Bin, <<"--bnd--">>) =/= nomatch).
