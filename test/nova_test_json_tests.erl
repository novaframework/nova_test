-module(nova_test_json_tests).
-include_lib("eunit/include/eunit.hrl").

-export([encode/1, decode/1]).

encode(Term) ->
    {ok, iolist_to_binary(["custom:", json:encode(Term)])}.

decode(<<"custom:", Json/binary>>) ->
    {ok, #{~"decoded_by" => ~"custom", ~"value" => json:decode(Json)}}.

defaults_to_otp_json_test() ->
    ?assertEqual(json, nova_test_json:json_lib()).

works_without_thoas_test_() ->
    {setup, fun hide_thoas/0, fun restore_thoas/1, fun(_) ->
        [
            ?_assertEqual(#{~"ok" => true}, nova_test:json(response(~"{\"ok\":true}"))),
            ?_assertMatch(
                #{body := ~"{\"a\":1}"},
                nova_test_req:with_json(#{a => 1}, nova_test_req:new(post, "/"))
            )
        ]
    end}.

configured_json_lib_test_() ->
    {setup, fun use_custom_json_lib/0, fun restore_json_lib/1, fun(_) ->
        [
            ?_assertEqual(?MODULE, nova_test_json:json_lib()),
            ?_assertEqual(~"custom:[1]", nova_test_json:encode([1])),
            ?_assertEqual(
                #{~"decoded_by" => ~"custom", ~"value" => [1]},
                nova_test:json(response(~"custom:[1]"))
            )
        ]
    end}.

decode_error_test_() ->
    {setup, fun use_tuple_error_lib/0, fun restore_json_lib/1, fun(_) ->
        ?_assertError({json_decode_failed, bad}, nova_test_json:decode(~"x"))
    end}.

otp_json_decode_error_test() ->
    ?assertError({json_decode_failed, _}, nova_test:json(response(~"{not json"))).

nova_env_json_lib_test_() ->
    {setup,
        fun() ->
            Previous = application:get_env(nova, json_lib),
            ok = application:set_env(nova, json_lib, ?MODULE),
            Previous
        end,
        fun
            ({ok, Lib}) -> application:set_env(nova, json_lib, Lib);
            (undefined) -> application:unset_env(nova, json_lib)
        end, fun(_) -> ?_assertEqual(?MODULE, nova_test_json:json_lib()) end}.

bootstrap_app_wins_over_nova_env_test_() ->
    {setup,
        fun() ->
            ok = application:set_env(nova, json_lib, json),
            use_custom_json_lib()
        end,
        fun(Previous) ->
            application:unset_env(nova, json_lib),
            restore_json_lib(Previous)
        end,
        fun(_) -> ?_assertEqual(?MODULE, nova_test_json:json_lib()) end}.

with_json_has_binary_keys_test() ->
    Req = nova_test_req:with_json(#{name => ~"alice", tags => [a]}, nova_test_req:new(post, "/")),
    ?assertEqual(#{~"name" => ~"alice", ~"tags" => [~"a"]}, maps:get(json, Req)).

delegates_to_nova_json_test_() ->
    {setup, fun load_fake_nova_json/0, fun unload_fake_nova_json/1, fun
        (fake) ->
            [
                ?_assertEqual(~"nova_json", nova_test_json:encode(#{})),
                ?_assertEqual(#{~"via" => ~"nova_json"}, nova_test_json:decode(~"{}")),
                ?_assertError({json_decode_failed, nope}, nova_test_json:decode(~"bad"))
            ];
        (real) ->
            [?_assertEqual(#{~"a" => 1}, nova_test_json:decode(~"{\"a\":1}"))]
    end}.

%% Nova 0.19 ships nova_json; on older Nova, load a stand-in for the test only.
load_fake_nova_json() ->
    case code:which(nova_json) of
        non_existing ->
            Forms = [
                {attribute, 1, module, nova_json},
                {attribute, 1, export, [{encode, 1}, {decode, 1}]},
                {function, 1, encode, 1, [
                    {clause, 1, [{var, 1, '_'}], [], [
                        {bin, 1, [{bin_element, 1, {string, 1, "nova_json"}, default, default}]}
                    ]}
                ]},
                {function, 1, decode, 1, [
                    {clause, 1,
                        [{bin, 1, [{bin_element, 1, {string, 1, "bad"}, default, default}]}], [], [
                            {tuple, 1, [{atom, 1, error}, {atom, 1, nope}]}
                        ]},
                    {clause, 1, [{var, 1, '_'}], [], [
                        {tuple, 1, [
                            {atom, 1, ok},
                            {map, 1, [
                                {map_field_assoc, 1,
                                    {bin, 1, [
                                        {bin_element, 1, {string, 1, "via"}, default, default}
                                    ]},
                                    {bin, 1, [
                                        {bin_element, 1, {string, 1, "nova_json"}, default, default}
                                    ]}}
                            ]}
                        ]}
                    ]}
                ]}
            ],
            {ok, nova_json, Bin} = compile:forms(Forms),
            true = is_binary(Bin),
            {module, nova_json} = code:load_binary(nova_json, "nova_json.erl", Bin),
            fake;
        _ ->
            real
    end.

unload_fake_nova_json(fake) ->
    _ = code:purge(nova_json),
    true = code:delete(nova_json),
    _ = code:purge(nova_json),
    ok;
unload_fake_nova_json(real) ->
    ok.

response(Body) ->
    #{status => 200, headers => [], body => Body}.

hide_thoas() ->
    case code:which(thoas) of
        Path when is_list(Path) ->
            Dir = filename:dirname(Path),
            true = is_list(Dir),
            true = code:del_path(Dir),
            _ = code:purge(thoas),
            _ = code:delete(thoas),
            {thoas, Dir};
        _ ->
            none
    end.

restore_thoas({thoas, Dir}) ->
    true = code:add_patha(Dir),
    ok;
restore_thoas(none) ->
    ok.

use_custom_json_lib() ->
    use_json_lib(?MODULE).

use_tuple_error_lib() ->
    use_json_lib(nova_test_json_error_lib).

use_json_lib(Lib) ->
    Previous = application:get_env(nova, bootstrap_application),
    ok = application:set_env(nova, bootstrap_application, nova_test_json_app),
    ok = application:set_env(nova_test_json_app, json_lib, Lib),
    Previous.

restore_json_lib(Previous) ->
    ok = application:unset_env(nova_test_json_app, json_lib),
    case Previous of
        {ok, App} -> application:set_env(nova, bootstrap_application, App);
        undefined -> application:unset_env(nova, bootstrap_application)
    end.
