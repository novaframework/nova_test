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
        ?_assertError({json_decode_failed, _, bad}, nova_test_json:decode(~"x"))
    end}.

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
