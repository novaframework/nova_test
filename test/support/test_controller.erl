-module(test_controller).

-export([
    get_json/1,
    post_json/1,
    get_status/1,
    get_redirect/1,
    echo_id/1,
    get_headers/1
]).

get_json(_Req) ->
    {json, #{<<"ok">> => true}}.

post_json(#{json := Body}) ->
    {json, 201, #{}, Body};
post_json(_Req) ->
    {status, 400}.

get_status(_Req) ->
    {status, 204}.

get_redirect(_Req) ->
    {redirect, <<"/test/json">>}.

echo_id(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{<<"id">> => Id}}.

get_headers(_Req) ->
    {json, 200, #{<<"x-custom">> => <<"test-value">>}, #{<<"ok">> => true}}.
