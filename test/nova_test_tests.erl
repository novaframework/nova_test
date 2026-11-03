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

%% cookie_header/2

cookie_header_empty_test() ->
    ?assertEqual([], nova_test:cookie_header("/", [])).

cookie_header_with_cookies_test() ->
    Config = nova_test:set_cookie(<<"session">>, <<"abc">>, []),
    ?assertEqual([{<<"cookie">>, <<"session=abc">>}], nova_test:cookie_header("/x", Config)).

cookie_header_format_test() ->
    Config = [{nova_test_cookies, #{<<"a">> => <<"1">>, <<"b">> => <<"2">>}}],
    ?assertEqual([{<<"cookie">>, <<"a=1; b=2">>}], nova_test:cookie_header("/", Config)).

%% Internal: merge_set_cookies

merge_set_cookies_empty_test() ->
    ?assertEqual(#{}, nova_test:merge_set_cookies([], #{})).

merge_set_cookies_single_test() ->
    Headers = [{"set-cookie", "name=val; Path=/"}],
    Result = nova_test:merge_set_cookies(Headers, #{}),
    ?assertEqual(#{<<"name">> => <<"val">>}, Result).

merge_set_cookies_non_cookie_test() ->
    Headers = [{"content-type", "text/html"}],
    ?assertEqual(#{}, nova_test:merge_set_cookies(Headers, #{})).

merge_set_cookies_multiple_test() ->
    Headers = [{"set-cookie", "a=1"}, {"set-cookie", "b=2"}],
    Result = nova_test:merge_set_cookies(Headers, #{}),
    ?assertEqual(#{<<"a">> => <<"1">>, <<"b">> => <<"2">>}, Result).

merge_set_cookies_case_insensitive_test() ->
    Headers = [{"Set-Cookie", "x=y"}],
    Result = nova_test:merge_set_cookies(Headers, #{}),
    ?assertEqual(#{<<"x">> => <<"y">>}, Result).

merge_set_cookies_malformed_test() ->
    Headers = [{"set-cookie", "noequals"}, {"set-cookie", "good=val"}],
    Result = nova_test:merge_set_cookies(Headers, #{}),
    ?assertEqual(#{<<"good">> => <<"val">>}, Result).

%% Internal: parse_set_cookie

parse_set_cookie_simple_test() ->
    ?assertEqual(
        {<<"name">>, <<"val">>, active, undefined}, nova_test:parse_set_cookie("name=val")
    ).

parse_set_cookie_with_attrs_test() ->
    ?assertEqual(
        {<<"sid">>, <<"abc">>, active, <<"/">>},
        nova_test:parse_set_cookie("sid=abc; Path=/; HttpOnly")
    ).

parse_set_cookie_malformed_test() ->
    ?assertEqual(error, nova_test:parse_set_cookie("noequals")).

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

%% Cookie expiry

save_cookies_max_age_zero_removes_test() ->
    Config0 = nova_test:set_cookie(~"session_id", ~"abc", []),
    Resp = #{
        status => 200,
        headers => [{"set-cookie", "session_id=abc; Max-Age=0; Path=/"}],
        body => <<>>
    },
    ?assertEqual(#{}, nova_test:cookies(nova_test:save_cookies(Resp, Config0))).

save_cookies_past_expires_removes_test() ->
    Config0 = nova_test:set_cookie(~"sid", ~"abc", []),
    Expired = "sid=; Expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/",
    Resp = #{status => 200, headers => [{"set-cookie", Expired}], body => <<>>},
    ?assertEqual(undefined, nova_test:cookie(~"sid", nova_test:save_cookies(Resp, Config0))).

save_cookies_future_expires_kept_test() ->
    Cookie = "sid=abc; Expires=Fri, 01 Jan 2100 00:00:00 GMT",
    Resp = #{status => 200, headers => [{"set-cookie", Cookie}], body => <<>>},
    ?assertEqual(~"abc", nova_test:cookie(~"sid", nova_test:save_cookies(Resp, []))).

max_age_wins_over_expires_test() ->
    Cookie = "sid=abc; Max-Age=60; Expires=Thu, 01 Jan 1970 00:00:00 GMT",
    ?assertEqual({~"sid", ~"abc", active, undefined}, nova_test:parse_set_cookie(Cookie)).

cookie_value_with_equals_test() ->
    ?assertEqual({~"t", ~"YWJj==", active, ~"/"}, nova_test:parse_set_cookie("t=YWJj==; Path=/")).

%% Sessions

session_carries_cookies_test() ->
    Session = nova_test:session([]),
    Resp = #{status => 200, headers => [{"set-cookie", "sid=abc"}], body => <<>>},
    ?assertEqual(Session, nova_test:save_cookies(Resp, Session)),
    ?assertEqual(~"abc", nova_test:cookie(~"sid", Session)),
    ?assertEqual([{~"cookie", ~"sid=abc"}], nova_test:cookie_header("/", Session)).

session_starts_with_config_cookies_test() ->
    Config = nova_test:set_cookie(~"a", ~"1", []),
    Session = nova_test:session(Config),
    ?assertEqual(#{~"a" => ~"1"}, nova_test:cookies(Session)),
    ?assertEqual(undefined, proplists:get_value(nova_test_cookies, Session)).

session_set_and_clear_test() ->
    Session = nova_test:session([]),
    ?assertEqual(Session, nova_test:set_cookie(~"a", ~"1", Session)),
    ?assertEqual(~"1", nova_test:cookie(~"a", Session)),
    ?assertEqual(Session, nova_test:clear_cookies(Session)),
    ?assertEqual(#{}, nova_test:cookies(Session)).

sessions_are_independent_test() ->
    Alice = nova_test:session([]),
    Bob = nova_test:session([]),
    _ = nova_test:set_cookie(~"sid", ~"alice", Alice),
    ?assertEqual(undefined, nova_test:cookie(~"sid", Bob)).

session_expired_cookie_removed_test() ->
    Session = nova_test:set_cookie(~"sid", ~"abc", nova_test:session([])),
    Resp = #{status => 200, headers => [{"set-cookie", "sid=abc; Max-Age=0"}], body => <<>>},
    _ = nova_test:save_cookies(Resp, Session),
    ?assertEqual(#{}, nova_test:cookies(Session)).

%% CSRF tokens

csrf_token_from_form_test() ->
    Html = ~"<form><input type=\"hidden\" name=\"_csrf_token\" value=\"abc+/=\"></form>",
    ?assertEqual(~"abc+/=", nova_test:csrf_token(Html)).

csrf_token_attribute_order_test() ->
    Html = ~"<input value='tok' type='hidden' name='_csrf_token' />",
    ?assertEqual(~"tok", nova_test:csrf_token(Html)).

csrf_token_skips_other_inputs_test() ->
    Html = ~"<input name=\"email\" value=\"a@b\"><input name=\"_csrf_token\" value=\"tok\">",
    ?assertEqual(~"tok", nova_test:csrf_token(Html)).

csrf_token_unescapes_html_test() ->
    Html = ~"<input name=\"_csrf_token\" value=\"a&#x2F;b&#x3D;&amp;\">",
    ?assertEqual(~"a/b=&", nova_test:csrf_token(Html)).

csrf_token_custom_field_test() ->
    Html = ~"<input name=\"_csrf_token\" value=\"no\"><input name=\"csrf\" value=\"yes\">",
    ?assertEqual(~"yes", nova_test:csrf_token(#{field => ~"csrf"}, Html)).

csrf_token_ignores_meta_test() ->
    Html = ~"<head><meta name=\"csrf-token\" content=\"meta-tok\"></head>",
    ?assertEqual(undefined, nova_test:csrf_token(Html)).

csrf_token_custom_header_test() ->
    Resp = #{status => 200, headers => [{"x-xsrf-token", "h"}], body => ~"{}"},
    ?assertEqual(undefined, nova_test:csrf_token(Resp)),
    ?assertEqual(~"h", nova_test:csrf_token(#{header => "x-xsrf-token"}, Resp)).

csrf_token_form_before_header_test() ->
    Resp = #{
        status => 200,
        headers => [{"x-csrf-token", "hdr"}],
        body => ~"<input name=\"_csrf_token\" value=\"form\">"
    },
    ?assertEqual(~"form", nova_test:csrf_token(Resp)).

csrf_token_from_header_test() ->
    Resp = #{status => 200, headers => [{"x-csrf-token", "hdr-tok"}], body => ~"{}"},
    ?assertEqual(~"hdr-tok", nova_test:csrf_token(Resp)).

csrf_token_from_response_body_test() ->
    Resp = #{status => 200, headers => [], body => ~"<input name=\"_csrf_token\" value=\"t\">"},
    ?assertEqual(~"t", nova_test:csrf_token(Resp)).

csrf_token_missing_test() ->
    ?assertEqual(undefined, nova_test:csrf_token(~"<form></form>")).

%% Form bodies

build_request_form_test() ->
    Result = nova_test:build_request("http://localhost/x", [], #{
        form => [{~"a", ~"1 2"}, {~"b", ~"&"}]
    }),
    ?assertEqual(
        {"http://localhost/x", [], "application/x-www-form-urlencoded", ~"a=1+2&b=%26"},
        Result
    ).

%% Lifecycle

request_without_start_test() ->
    ?assertError({nova_test_not_started, _}, nova_test:get("/", [])).

start_unknown_app_test() ->
    ?assertError(
        {nova_test_start_failed, #{app := nova_test_no_such_app}},
        nova_test:start(nova_test_no_such_app)
    ).

%% Session cookie paths (RFC 6265)

default_path_test_() ->
    [
        ?_assertEqual(~"/", nova_test:default_path(~"/")),
        ?_assertEqual(~"/", nova_test:default_path(~"/login")),
        ?_assertEqual(~"/admin", nova_test:default_path(~"/admin/users")),
        ?_assertEqual(~"/admin", nova_test:default_path(~"/admin/")),
        ?_assertEqual(~"/", nova_test:default_path(~""))
    ].

path_match_test_() ->
    [
        ?_assert(nova_test:path_match(~"/admin", ~"/admin")),
        ?_assert(nova_test:path_match(~"/admin/users", ~"/admin")),
        ?_assert(nova_test:path_match(~"/admin/users", ~"/")),
        ?_assert(nova_test:path_match(~"/admin/users", ~"/admin/")),
        ?_assertNot(nova_test:path_match(~"/administrator", ~"/admin")),
        ?_assertNot(nova_test:path_match(~"/", ~"/admin"))
    ].

session_respects_cookie_path_test() ->
    Session = nova_test:session([]),
    Resp = #{
        status => 200,
        headers => [{"set-cookie", "admin=1; Path=/admin"}, {"set-cookie", "all=2; Path=/"}],
        body => <<>>
    },
    _ = nova_test:save_cookies(Resp, Session),
    ?assertEqual([{~"cookie", ~"all=2"}], nova_test:cookie_header("/posts", Session)),
    ?assertEqual(
        [{~"cookie", ~"admin=1; all=2"}], nova_test:cookie_header("/admin/users?x=1", Session)
    ).

session_same_name_different_paths_test() ->
    Session = nova_test:session([]),
    Resp = #{
        status => 200,
        headers => [{"set-cookie", "t=root; Path=/"}, {"set-cookie", "t=admin; Path=/admin"}],
        body => <<>>
    },
    _ = nova_test:save_cookies(Resp, Session),
    ?assertEqual([{~"cookie", ~"t=admin; t=root"}], nova_test:cookie_header("/admin/x", Session)),
    ?assertEqual(~"admin", nova_test:cookie(~"t", Session)),
    Expire = #{
        status => 200, headers => [{"set-cookie", "t=; Path=/admin; Max-Age=0"}], body => <<>>
    },
    _ = nova_test:save_cookies(Expire, Session),
    ?assertEqual([{~"cookie", ~"t=root"}], nova_test:cookie_header("/admin/x", Session)).

session_response_updates_only_named_cookies_test() ->
    Session = nova_test:set_cookie(~"keep", ~"1", nova_test:session([])),
    Resp = #{status => 200, headers => [{"set-cookie", "new=2"}], body => <<>>},
    _ = nova_test:save_cookies(Resp, Session),
    ?assertEqual(#{~"keep" => ~"1", ~"new" => ~"2"}, nova_test:cookies(Session)).

%% Request paths

invalid_path_test_() ->
    Config = [{nova_test_port, 1}],
    [
        ?_assertError(
            {nova_test_invalid_path, #{path := "@127.0.0.1:1/x", reason := _}},
            nova_test:get("@127.0.0.1:1/x", Config)
        ),
        ?_assertError({nova_test_invalid_path, #{path := "x"}}, nova_test:get("x", Config)),
        ?_assertError(
            {nova_test_invalid_path, #{path := "@127.0.0.1:1/x"}},
            nova_test:post("@127.0.0.1:1/x", #{json => #{}}, Config)
        ),
        ?_assertError(
            {nova_test_invalid_path, #{path := ""}}, nova_test:request(delete, "", #{}, Config)
        )
    ].
