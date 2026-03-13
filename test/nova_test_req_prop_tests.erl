-module(nova_test_req_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property-based tests for nova_test_req

%% --- Generators ---

http_method() ->
    oneof([get, post, put, patch, delete, head, options]).

path() ->
    ?LET(
        Segments,
        non_empty(list(path_segment())),
        list_to_binary(["/" | lists:join("/", Segments)])
    ).

path_segment() ->
    ?LET(
        S,
        non_empty(list(oneof([range($a, $z), range($0, $9), $-]))),
        list_to_binary(S)
    ).

header_name() ->
    ?LET(
        Parts,
        non_empty(list(header_word())),
        list_to_binary(lists:join("-", Parts))
    ).

header_word() ->
    ?LET(
        S,
        non_empty(list(range($a, $z))),
        list_to_binary(S)
    ).

header_value() ->
    ?LET(
        S,
        non_empty(list(range($a, $z))),
        list_to_binary(S)
    ).

query_key() ->
    ?LET(
        S,
        non_empty(list(range($a, $z))),
        list_to_binary(S)
    ).

query_value() ->
    ?LET(
        S,
        non_empty(list(oneof([range($a, $z), range($0, $9)]))),
        list_to_binary(S)
    ).

%% non_empty/1 doesn't work with map/2 in PropEr, so use SUCHTHAT
non_empty_query_map() ->
    ?SUCHTHAT(M, map(query_key(), query_value()), map_size(M) > 0).

json_text() ->
    ?LET(
        S,
        non_empty(list(oneof([range($a, $z), range($A, $Z), range($0, $9), $_, $-]))),
        list_to_binary(S)
    ).

ip_byte() -> range(0, 255).

port_number() -> range(1, 65535).

%% --- Properties ---

%% new/2 always returns a map with the required cowboy_req keys
prop_new_has_required_keys() ->
    ?FORALL(
        {Method, Path},
        {http_method(), path()},
        begin
            Req = nova_test_req:new(Method, Path),
            lists:all(
                fun(K) -> maps:is_key(K, Req) end,
                [method, path, host, port, scheme, qs, version, headers, bindings, peer]
            )
        end
    ).

%% Method is always an uppercase binary
prop_method_uppercase_binary() ->
    ?FORALL(
        Method,
        http_method(),
        begin
            Req = nova_test_req:new(Method, <<"/test">>),
            M = maps:get(method, Req),
            is_binary(M) andalso M =:= list_to_binary(string:uppercase(binary_to_list(M)))
        end
    ).

%% Path is preserved exactly
prop_path_preserved() ->
    ?FORALL(
        Path,
        path(),
        begin
            Req = nova_test_req:new(get, Path),
            maps:get(path, Req) =:= Path
        end
    ).

%% String and binary paths produce the same request
prop_string_binary_path_equiv() ->
    ?FORALL(
        Path,
        path(),
        begin
            ReqBin = nova_test_req:new(get, Path),
            ReqStr = nova_test_req:new(get, binary_to_list(Path)),
            maps:get(path, ReqBin) =:= maps:get(path, ReqStr)
        end
    ).

%% Adding two distinct headers preserves both
prop_headers_accumulate() ->
    ?FORALL(
        {N1, V1, N2, V2},
        {header_name(), header_value(), header_name(), header_value()},
        ?IMPLIES(
            N1 =/= N2,
            begin
                Req = nova_test_req:with_header(
                    N2,
                    V2,
                    nova_test_req:with_header(
                        N1,
                        V1,
                        nova_test_req:new(get, <<"/t">>)
                    )
                ),
                Headers = maps:get(headers, Req),
                maps:get(N1, Headers) =:= V1 andalso maps:get(N2, Headers) =:= V2
            end
        )
    ).

%% Overwriting a header keeps only the latest value
prop_header_last_write_wins() ->
    ?FORALL(
        {Name, V1, V2},
        {header_name(), header_value(), header_value()},
        begin
            Req = nova_test_req:with_header(
                Name,
                V2,
                nova_test_req:with_header(
                    Name,
                    V1,
                    nova_test_req:new(get, <<"/t">>)
                )
            ),
            maps:get(Name, maps:get(headers, Req)) =:= V2
        end
    ).

%% Bindings are set exactly
prop_bindings_exact() ->
    ?FORALL(
        Bindings,
        map(query_key(), query_value()),
        begin
            Req = nova_test_req:with_bindings(Bindings, nova_test_req:new(get, <<"/t">>)),
            maps:get(bindings, Req) =:= Bindings
        end
    ).

%% Every key=value pair from the params map appears in the query string
prop_query_contains_all_pairs() ->
    ?FORALL(
        Params,
        non_empty_query_map(),
        begin
            Req = nova_test_req:with_query(Params, nova_test_req:new(get, <<"/s">>)),
            QS = maps:get(qs, Req),
            maps:fold(
                fun(K, V, Acc) ->
                    Pair = <<K/binary, "=", V/binary>>,
                    Acc andalso binary:match(QS, Pair) =/= nomatch
                end,
                true,
                Params
            )
        end
    ).

%% Query string has exactly (N-1) ampersands for N params
prop_query_ampersand_count() ->
    ?FORALL(
        Params,
        non_empty_query_map(),
        begin
            Req = nova_test_req:with_query(Params, nova_test_req:new(get, <<"/s">>)),
            QS = maps:get(qs, Req),
            length(binary:matches(QS, <<"&">>)) =:= maps:size(Params) - 1
        end
    ).

%% Body round-trips through with_body
prop_body_roundtrip() ->
    ?FORALL(
        Body,
        binary(),
        begin
            Req = nova_test_req:with_body(Body, nova_test_req:new(post, <<"/up">>)),
            maps:get(body, Req) =:= Body
        end
    ).

%% Peer round-trips through with_peer
prop_peer_roundtrip() ->
    ?FORALL(
        {A, B, C, D, Port},
        {ip_byte(), ip_byte(), ip_byte(), ip_byte(), port_number()},
        begin
            Peer = {{A, B, C, D}, Port},
            Req = nova_test_req:with_peer(Peer, nova_test_req:new(get, <<"/i">>)),
            maps:get(peer, Req) =:= Peer
        end
    ).

%% Auth data round-trips
prop_auth_data_roundtrip() ->
    ?FORALL(
        AuthData,
        map(atom(), binary()),
        begin
            Req = nova_test_req:with_auth_data(AuthData, nova_test_req:new(get, <<"/a">>)),
            maps:get(auth_data, Req) =:= AuthData
        end
    ).

%% with_json always sets content-type to application/json
prop_json_sets_content_type() ->
    ?FORALL(
        Json,
        map(json_text(), json_text()),
        begin
            Req = nova_test_req:with_json(Json, nova_test_req:new(post, <<"/api">>)),
            maps:get(<<"content-type">>, maps:get(headers, Req)) =:= <<"application/json">>
        end
    ).

%% with_json stores original map in json key
prop_json_stores_original() ->
    ?FORALL(
        Json,
        map(json_text(), json_text()),
        begin
            Req = nova_test_req:with_json(Json, nova_test_req:new(post, <<"/api">>)),
            maps:get(json, Req) =:= Json
        end
    ).

%% --- EUnit wrappers ---

run_prop(Prop) ->
    ?assert(proper:quickcheck(Prop, [quiet, {numtests, 100}])).

prop_new_has_required_keys_test() ->
    run_prop(prop_new_has_required_keys()).

prop_method_uppercase_binary_test() ->
    run_prop(prop_method_uppercase_binary()).

prop_path_preserved_test() ->
    run_prop(prop_path_preserved()).

prop_string_binary_path_equiv_test() ->
    run_prop(prop_string_binary_path_equiv()).

prop_headers_accumulate_test() ->
    run_prop(prop_headers_accumulate()).

prop_header_last_write_wins_test() ->
    run_prop(prop_header_last_write_wins()).

prop_bindings_exact_test() ->
    run_prop(prop_bindings_exact()).

prop_query_contains_all_pairs_test() ->
    run_prop(prop_query_contains_all_pairs()).

prop_query_ampersand_count_test() ->
    run_prop(prop_query_ampersand_count()).

prop_body_roundtrip_test() ->
    run_prop(prop_body_roundtrip()).

prop_peer_roundtrip_test() ->
    run_prop(prop_peer_roundtrip()).

prop_auth_data_roundtrip_test() ->
    run_prop(prop_auth_data_roundtrip()).

prop_json_sets_content_type_test() ->
    run_prop(prop_json_sets_content_type()).

prop_json_stores_original_test() ->
    run_prop(prop_json_stores_original()).
