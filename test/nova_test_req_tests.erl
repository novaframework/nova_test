-module(nova_test_req_tests).
-include_lib("eunit/include/eunit.hrl").

%% new/2 — HTTP methods

new_get_test() ->
    Req = nova_test_req:new(get, <<"/users">>),
    ?assertEqual(<<"GET">>, maps:get(method, Req)),
    ?assertEqual(<<"/users">>, maps:get(path, Req)).

new_post_test() ->
    ?assertEqual(<<"POST">>, maps:get(method, nova_test_req:new(post, <<"/users">>))).

new_put_test() ->
    ?assertEqual(<<"PUT">>, maps:get(method, nova_test_req:new(put, <<"/users/1">>))).

new_patch_test() ->
    ?assertEqual(<<"PATCH">>, maps:get(method, nova_test_req:new(patch, <<"/users/1">>))).

new_delete_test() ->
    ?assertEqual(<<"DELETE">>, maps:get(method, nova_test_req:new(delete, <<"/users/1">>))).

new_head_test() ->
    ?assertEqual(<<"HEAD">>, maps:get(method, nova_test_req:new(head, <<"/health">>))).

new_options_test() ->
    ?assertEqual(<<"OPTIONS">>, maps:get(method, nova_test_req:new(options, <<"/users">>))).

new_custom_method_test() ->
    ?assertEqual(<<"TRACE">>, maps:get(method, nova_test_req:new(trace, <<"/debug">>))).

%% new/2 — string path conversion

string_path_converted_test() ->
    Req = nova_test_req:new(get, "/api/users"),
    ?assertEqual(<<"/api/users">>, maps:get(path, Req)).

%% new/2 — defaults

defaults_test() ->
    Req = nova_test_req:new(get, <<"/test">>),
    ?assertEqual(<<"localhost">>, maps:get(host, Req)),
    ?assertEqual(8080, maps:get(port, Req)),
    ?assertEqual(<<"http">>, maps:get(scheme, Req)),
    ?assertEqual(<<>>, maps:get(qs, Req)),
    ?assertEqual('HTTP/1.1', maps:get(version, Req)),
    ?assertEqual(#{}, maps:get(headers, Req)),
    ?assertEqual(#{}, maps:get(bindings, Req)),
    ?assertMatch({{127, 0, 0, 1}, _}, maps:get(peer, Req)).

%% with_bindings/2

bindings_set_test() ->
    Req = nova_test_req:with_bindings(#{id => <<"1">>}, nova_test_req:new(get, <<"/users/1">>)),
    ?assertEqual(#{id => <<"1">>}, maps:get(bindings, Req)).

bindings_replace_test() ->
    Req0 = nova_test_req:new(get, <<"/users/1">>),
    Req1 = nova_test_req:with_bindings(#{id => <<"1">>}, Req0),
    Req2 = nova_test_req:with_bindings(#{id => <<"2">>}, Req1),
    ?assertEqual(#{id => <<"2">>}, maps:get(bindings, Req2)).

%% with_json/2

json_content_type_test() ->
    Req = nova_test_req:with_json(#{<<"k">> => <<"v">>}, nova_test_req:new(post, <<"/api">>)),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, maps:get(headers, Req))).

json_body_is_binary_test() ->
    Req = nova_test_req:with_json(#{<<"k">> => <<"v">>}, nova_test_req:new(post, <<"/api">>)),
    ?assert(is_binary(maps:get(body, Req))).

json_stores_original_map_test() ->
    Json = #{<<"name">> => <<"Bob">>},
    Req = nova_test_req:with_json(Json, nova_test_req:new(post, <<"/api">>)),
    ?assertEqual(Json, maps:get(json, Req)).

json_preserves_existing_headers_test() ->
    Req0 = nova_test_req:new(post, <<"/api">>),
    Req1 = nova_test_req:with_header(<<"x-custom">>, <<"val">>, Req0),
    Req2 = nova_test_req:with_json(#{<<"k">> => <<"v">>}, Req1),
    Headers = maps:get(headers, Req2),
    ?assertEqual(<<"val">>, maps:get(<<"x-custom">>, Headers)),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)).

%% with_header/3

header_added_test() ->
    Req = nova_test_req:with_header(
        <<"authorization">>, <<"Bearer abc">>, nova_test_req:new(get, <<"/test">>)
    ),
    ?assertEqual(<<"Bearer abc">>, maps:get(<<"authorization">>, maps:get(headers, Req))).

multiple_headers_test() ->
    Req0 = nova_test_req:new(get, <<"/test">>),
    Req1 = nova_test_req:with_header(<<"accept">>, <<"application/json">>, Req0),
    Req2 = nova_test_req:with_header(<<"authorization">>, <<"Bearer abc">>, Req1),
    Headers = maps:get(headers, Req2),
    ?assertEqual(<<"application/json">>, maps:get(<<"accept">>, Headers)),
    ?assertEqual(<<"Bearer abc">>, maps:get(<<"authorization">>, Headers)).

header_overwrite_test() ->
    Req0 = nova_test_req:new(get, <<"/test">>),
    Req1 = nova_test_req:with_header(<<"accept">>, <<"text/html">>, Req0),
    Req2 = nova_test_req:with_header(<<"accept">>, <<"application/json">>, Req1),
    ?assertEqual(<<"application/json">>, maps:get(<<"accept">>, maps:get(headers, Req2))).

%% with_auth_data/2

auth_data_test() ->
    AuthData = #{user_id => 1, role => admin},
    Req = nova_test_req:with_auth_data(AuthData, nova_test_req:new(get, <<"/admin">>)),
    ?assertEqual(AuthData, maps:get(auth_data, Req)).

%% with_query/2

query_single_param_test() ->
    Req = nova_test_req:with_query(
        #{<<"q">> => <<"erlang">>}, nova_test_req:new(get, <<"/search">>)
    ),
    ?assertEqual(<<"q=erlang">>, maps:get(qs, Req)).

query_multiple_params_test() ->
    Req = nova_test_req:with_query(
        #{<<"q">> => <<"erlang">>, <<"page">> => <<"1">>},
        nova_test_req:new(get, <<"/search">>)
    ),
    QS = maps:get(qs, Req),
    ?assert(binary:match(QS, <<"q=erlang">>) =/= nomatch),
    ?assert(binary:match(QS, <<"page=1">>) =/= nomatch),
    ?assert(binary:match(QS, <<"&">>) =/= nomatch).

query_empty_params_test() ->
    Req = nova_test_req:with_query(#{}, nova_test_req:new(get, <<"/search">>)),
    ?assertEqual(<<>>, maps:get(qs, Req)).

query_integer_value_test() ->
    Req = nova_test_req:with_query(#{<<"page">> => 2}, nova_test_req:new(get, <<"/search">>)),
    ?assertEqual(<<"page=2">>, maps:get(qs, Req)).

query_atom_value_test() ->
    Req = nova_test_req:with_query(#{<<"fmt">> => json}, nova_test_req:new(get, <<"/search">>)),
    ?assertEqual(<<"fmt=json">>, maps:get(qs, Req)).

query_string_key_value_test() ->
    Req = nova_test_req:with_query(#{<<"lang">> => "erlang"}, nova_test_req:new(get, <<"/s">>)),
    ?assertEqual(<<"lang=erlang">>, maps:get(qs, Req)).

%% with_body/2

body_set_test() ->
    Req = nova_test_req:with_body(<<"raw">>, nova_test_req:new(post, <<"/upload">>)),
    ?assertEqual(<<"raw">>, maps:get(body, Req)).

%% with_peer/2

peer_set_test() ->
    Peer = {{10, 0, 0, 1}, 9999},
    Req = nova_test_req:with_peer(Peer, nova_test_req:new(get, <<"/info">>)),
    ?assertEqual(Peer, maps:get(peer, Req)).

%% with_multipart/2

multipart_sets_body_test() ->
    Fields = [{field, <<"name">>, <<"test">>}],
    Req = nova_test_req:with_multipart(Fields, nova_test_req:new(post, <<"/upload">>)),
    ?assert(is_binary(maps:get(body, Req))),
    ?assertEqual(Fields, maps:get(multipart, Req)).

multipart_sets_content_type_test() ->
    Fields = [{field, <<"name">>, <<"test">>}],
    Req = nova_test_req:with_multipart(Fields, nova_test_req:new(post, <<"/upload">>)),
    CT = maps:get(<<"content-type">>, maps:get(headers, Req)),
    ?assertMatch(<<"multipart/form-data; boundary=", _/binary>>, CT).

multipart_file_body_test() ->
    Fields = [{file, <<"doc">>, <<"test.txt">>, <<"text/plain">>, <<"hello">>}],
    Req = nova_test_req:with_multipart(Fields, nova_test_req:new(post, <<"/upload">>)),
    Body = maps:get(body, Req),
    ?assert(binary:match(Body, <<"filename=\"test.txt\"">>) =/= nomatch),
    ?assert(binary:match(Body, <<"hello">>) =/= nomatch).

multipart_preserves_headers_test() ->
    Req0 = nova_test_req:with_header(
        <<"x-custom">>, <<"val">>, nova_test_req:new(post, <<"/upload">>)
    ),
    Req1 = nova_test_req:with_multipart([{field, <<"k">>, <<"v">>}], Req0),
    Headers = maps:get(headers, Req1),
    ?assertEqual(<<"val">>, maps:get(<<"x-custom">>, Headers)).

%% with_cookies/2

cookies_single_test() ->
    Req = nova_test_req:with_cookies(
        #{<<"session">> => <<"abc">>}, nova_test_req:new(get, <<"/">>)
    ),
    Cookie = maps:get(<<"cookie">>, maps:get(headers, Req)),
    ?assertEqual(<<"session=abc">>, Cookie).

cookies_multiple_test() ->
    Cookies = #{<<"a">> => <<"1">>, <<"b">> => <<"2">>},
    Req = nova_test_req:with_cookies(Cookies, nova_test_req:new(get, <<"/">>)),
    Cookie = maps:get(<<"cookie">>, maps:get(headers, Req)),
    ?assert(binary:match(Cookie, <<"a=1">>) =/= nomatch),
    ?assert(binary:match(Cookie, <<"b=2">>) =/= nomatch).

cookies_preserves_headers_test() ->
    Req0 = nova_test_req:with_header(<<"accept">>, <<"*/*">>, nova_test_req:new(get, <<"/">>)),
    Req1 = nova_test_req:with_cookies(#{<<"s">> => <<"v">>}, Req0),
    Headers = maps:get(headers, Req1),
    ?assertEqual(<<"*/*">>, maps:get(<<"accept">>, Headers)).

%% Chaining

full_chain_test() ->
    Req = nova_test_req:with_auth_data(
        #{user_id => 1},
        nova_test_req:with_header(
            <<"authorization">>,
            <<"Bearer t">>,
            nova_test_req:with_json(
                #{<<"name">> => <<"Alice">>},
                nova_test_req:new(post, <<"/api/users">>)
            )
        )
    ),
    ?assertEqual(<<"POST">>, maps:get(method, Req)),
    ?assertEqual(#{<<"name">> => <<"Alice">>}, maps:get(json, Req)),
    ?assertEqual(#{user_id => 1}, maps:get(auth_data, Req)),
    Headers = maps:get(headers, Req),
    ?assertEqual(<<"Bearer t">>, maps:get(<<"authorization">>, Headers)),
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Headers)).
