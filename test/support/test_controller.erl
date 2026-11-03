-module(test_controller).

-export([
    get_json/1,
    post_json/1,
    get_status/1,
    get_redirect/1,
    echo_id/1,
    get_headers/1,
    set_cookie/1,
    get_cookie/1,
    upload/1,
    echo_query/1,
    csrf_form/1,
    csrf_submit/1,
    log_out/1
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

set_cookie(Req) ->
    Req1 = cowboy_req:set_resp_cookie(<<"test_session">>, <<"abc123">>, Req, #{path => <<"/">>}),
    {json, 200, #{}, Req1, #{<<"ok">> => true}}.

get_cookie(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    CookieMap = maps:from_list(Cookies),
    {json, CookieMap}.

upload(Req) ->
    {Parts, _Req2} = read_multipart(Req, []),
    {json, #{<<"parts">> => Parts}}.

echo_query(#{parsed_qs := Qs}) ->
    {json, Qs}.

csrf_form(#{csrf_token := Token}) ->
    Html = <<"<form method=\"post\"><input type=\"hidden\" name=\"_csrf_token\" value=\"",
        Token/binary, "\"></form>">>,
    {status, 200, #{<<"content-type">> => <<"text/html">>}, Html};
csrf_form(_Req) ->
    {status, 500}.

csrf_submit(#{params := Params}) ->
    {json, maps:remove(<<"_csrf_token">>, Params)}.

log_out(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {status, 204, #{}, <<>>, Req1}.

%% Internal

read_multipart(Req, Acc) ->
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            {ok, Body, Req3} = cowboy_req:read_part_body(Req2),
            Name = part_param(<<"name">>, Headers),
            Filename = part_param(<<"filename">>, Headers),
            Part = #{
                <<"name">> => Name,
                <<"size">> => byte_size(Body),
                <<"body">> => Body
            },
            Part2 = case Filename of
                undefined -> Part;
                _ -> Part#{<<"filename">> => Filename}
            end,
            read_multipart(Req3, [Part2 | Acc]);
        {done, Req2} ->
            {lists:reverse(Acc), Req2}
    end.

part_param(Param, Headers) ->
    case maps:find(<<"content-disposition">>, Headers) of
        {ok, Disposition} ->
            Search = <<Param/binary, "=\"">>,
            case binary:match(Disposition, Search) of
                {Start, Len} ->
                    After = Start + Len,
                    Rest = byte_size(Disposition) - After,
                    case binary:match(Disposition, <<"\"">>, [{scope, {After, Rest}}]) of
                        {End, _} -> binary:part(Disposition, After, End - After);
                        nomatch -> undefined
                    end;
                nomatch ->
                    undefined
            end;
        false ->
            undefined
    end.
