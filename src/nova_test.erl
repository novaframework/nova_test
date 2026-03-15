-module(nova_test).
-moduledoc """
HTTP client for integration testing Nova applications.

Start your Nova app, make HTTP requests, and assert on responses.
Manages cookies across requests via the CT Config proplist.

```erlang
Config = nova_test:start(my_app),
{ok, Resp} = nova_test:get("/api/users", Config),
?assertStatus(200, Resp),
?assertJson(#{<<"users">> := _}, Resp),
nova_test:stop(Config).
```
""".

-export([
    start/1,
    start/2,
    stop/1,
    get/2,
    get/3,
    post/2,
    post/3,
    put/3,
    patch/3,
    delete/2,
    delete/3,
    request/4,
    status/1,
    body/1,
    json/1,
    headers/1,
    header/2,
    save_cookies/2,
    cookies/1,
    cookie/2,
    set_cookie/3,
    clear_cookies/1,
    enable_logging/1,
    disable_logging/1,
    build_multipart_body/2
]).

-export_type([response/0, opts/0, multipart_field/0]).

-type response() :: #{
    status := integer(),
    headers := [{string(), string()}],
    body := binary()
}.

-type multipart_field() ::
    {field, Name :: binary(), Value :: binary()}
    | {file, Name :: binary(), Filename :: binary(), ContentType :: binary(), Data :: binary()}.

-type opts() :: #{
    headers => [{binary(), binary()}],
    json => map(),
    body => binary(),
    content_type => string(),
    multipart => [multipart_field()]
}.

%% Lifecycle

-spec start(App :: atom()) -> [{atom(), term()}].
start(App) ->
    start(App, []).

-spec start(App :: atom(), Config :: [{atom(), term()}]) -> [{atom(), term()}].
start(App, Config) ->
    application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(App),
    Port = get_port(),
    [{nova_test_port, Port}, {nova_test_app, App} | Config].

-spec stop(Config :: [{atom(), term()}]) -> ok.
stop(Config) ->
    App = proplists:get_value(nova_test_app, Config),
    application:stop(App),
    ok.

%% HTTP methods

-spec get(Path :: string(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
get(Path, Config) ->
    request(get, Path, #{}, Config).

-spec get(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
get(Path, Opts, Config) ->
    request(get, Path, Opts, Config).

-spec post(Path :: string(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
post(Path, Config) ->
    request(post, Path, #{}, Config).

-spec post(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
post(Path, Opts, Config) ->
    request(post, Path, Opts, Config).

-spec put(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
put(Path, Opts, Config) ->
    request(put, Path, Opts, Config).

-spec patch(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
patch(Path, Opts, Config) ->
    request(patch, Path, Opts, Config).

-spec delete(Path :: string(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
delete(Path, Config) ->
    request(delete, Path, #{}, Config).

-spec delete(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
delete(Path, Opts, Config) ->
    request(delete, Path, Opts, Config).

-spec request(Method :: atom(), Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
request(Method, Path, Opts, Config) ->
    Port = proplists:get_value(nova_test_port, Config),
    URL = "http://localhost:" ++ integer_to_list(Port) ++ Path,
    Headers = build_headers(Opts) ++ cookie_header(Config),
    HttpMethod = Method,
    Request = build_request(URL, Headers, Opts),
    HttpOpts = [{autoredirect, false}],
    ReqOpts = [{body_format, binary}],
    case httpc:request(HttpMethod, Request, HttpOpts, ReqOpts) of
        {ok, {{_, Status, _}, RespHeaders, Body}} ->
            Response = #{
                status => Status,
                headers => RespHeaders,
                body => Body
            },
            maybe_log(Method, Path, Opts, Response, Config),
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end.

%% Response accessors

-spec status(response()) -> integer().
status(#{status := Status}) -> Status.

-spec body(response()) -> binary().
body(#{body := Body}) -> Body.

-spec json(response()) -> map().
json(#{body := Body}) ->
    JsonLib = json_lib(),
    {ok, Decoded} = JsonLib:decode(Body),
    Decoded.

-spec headers(response()) -> [{string(), string()}].
headers(#{headers := Headers}) -> Headers.

-spec header(Name :: string(), response()) -> string() | undefined.
header(Name, #{headers := Headers}) ->
    LowerName = string:lowercase(Name),
    case lists:keyfind(LowerName, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, Value} -> Value;
        false -> undefined
    end.

%% Cookie management

-spec save_cookies(response(), [{atom(), term()}]) -> [{atom(), term()}].
save_cookies(#{headers := Headers}, Config) ->
    SetCookies = extract_set_cookies(Headers),
    Existing = proplists:get_value(nova_test_cookies, Config, #{}),
    Merged = maps:merge(Existing, SetCookies),
    [{nova_test_cookies, Merged} | proplists:delete(nova_test_cookies, Config)].

-spec cookies(Config :: [{atom(), term()}]) -> map().
cookies(Config) ->
    proplists:get_value(nova_test_cookies, Config, #{}).

-spec cookie(Name :: binary(), Config :: [{atom(), term()}]) -> binary() | undefined.
cookie(Name, Config) ->
    maps:get(Name, cookies(Config), undefined).

-spec set_cookie(Name :: binary(), Value :: binary(), Config :: [{atom(), term()}]) ->
    [{atom(), term()}].
set_cookie(Name, Value, Config) ->
    Existing = proplists:get_value(nova_test_cookies, Config, #{}),
    Updated = Existing#{Name => Value},
    [{nova_test_cookies, Updated} | proplists:delete(nova_test_cookies, Config)].

-spec clear_cookies(Config :: [{atom(), term()}]) -> [{atom(), term()}].
clear_cookies(Config) ->
    [{nova_test_cookies, #{}} | proplists:delete(nova_test_cookies, Config)].

%% Logging

-spec enable_logging(Config :: [{atom(), term()}]) -> [{atom(), term()}].
enable_logging(Config) ->
    [{nova_test_logging, true} | proplists:delete(nova_test_logging, Config)].

-spec disable_logging(Config :: [{atom(), term()}]) -> [{atom(), term()}].
disable_logging(Config) ->
    [{nova_test_logging, false} | proplists:delete(nova_test_logging, Config)].

%% Multipart

-spec build_multipart_body([multipart_field()], Boundary :: binary()) -> iolist().
build_multipart_body(Fields, Boundary) ->
    Parts = [multipart_part(Field, Boundary) || Field <- Fields],
    [Parts, <<"--", Boundary/binary, "--\r\n">>].

%% Internal

get_port() ->
    Config = application:get_env(nova, cowboy_configuration, #{}),
    maps:get(port, Config, 8080).

build_headers(#{headers := Headers}) ->
    [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers];
build_headers(_) ->
    [].

build_request(URL, Headers, #{json := Json}) ->
    JsonLib = json_lib(),
    Body = JsonLib:encode(Json),
    BodyStr =
        case Body of
            B when is_binary(B) -> binary_to_list(B);
            B when is_list(B) -> B
        end,
    {URL, Headers, "application/json", BodyStr};
build_request(URL, Headers, #{multipart := Fields}) ->
    Boundary = generate_boundary(),
    Body = iolist_to_binary(build_multipart_body(Fields, Boundary)),
    CT = "multipart/form-data; boundary=" ++ binary_to_list(Boundary),
    {URL, Headers, CT, binary_to_list(Body)};
build_request(URL, Headers, #{body := Body, content_type := CT}) ->
    BodyStr = binary_to_list(Body),
    {URL, Headers, CT, BodyStr};
build_request(URL, Headers, #{body := Body}) ->
    BodyStr = binary_to_list(Body),
    {URL, Headers, "text/plain", BodyStr};
build_request(URL, Headers, _) ->
    {URL, Headers}.

json_lib() ->
    try
        nova:get_env(json_lib, thoas)
    catch
        _:_ -> thoas
    end.

cookie_header(Config) ->
    case proplists:get_value(nova_test_cookies, Config, #{}) of
        Cookies when map_size(Cookies) =:= 0 ->
            [];
        Cookies ->
            CookieStr = maps:fold(
                fun(Name, Value, Acc) ->
                    Pair = binary_to_list(Name) ++ "=" ++ binary_to_list(Value),
                    case Acc of
                        "" -> Pair;
                        _ -> Acc ++ "; " ++ Pair
                    end
                end,
                "",
                Cookies
            ),
            [{"cookie", CookieStr}]
    end.

extract_set_cookies(Headers) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case string:lowercase(Key) of
                "set-cookie" ->
                    case parse_cookie(Value) of
                        {Name, Val} -> Acc#{Name => Val};
                        error -> Acc
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        Headers
    ).

parse_cookie(CookieStr) ->
    case string:split(CookieStr, ";") of
        [NameValue | _] ->
            case string:split(string:trim(NameValue), "=") of
                [Name, Value] ->
                    {list_to_binary(string:trim(Name)), list_to_binary(string:trim(Value))};
                _ ->
                    error
            end;
        _ ->
            error
    end.

maybe_log(Method, Path, Opts, #{status := Status, headers := RespHeaders, body := Body}, Config) ->
    case proplists:get_value(nova_test_logging, Config, false) of
        true ->
            ReqBody =
                case Opts of
                    #{json := Json} ->
                        JsonLib = json_lib(),
                        JsonLib:encode(Json);
                    #{body := B} ->
                        B;
                    _ ->
                        <<>>
                end,
            ct:pal(
                "~n=== nova_test ~s ~s ===~n"
                "Request headers: ~p~n"
                "Request body: ~ts~n"
                "Response status: ~p~n"
                "Response headers: ~p~n"
                "Response body: ~ts~n",
                [
                    string:uppercase(atom_to_list(Method)),
                    Path,
                    build_headers(Opts),
                    ReqBody,
                    Status,
                    RespHeaders,
                    Body
                ]
            );
        false ->
            ok
    end.

generate_boundary() ->
    Int = erlang:unique_integer([positive]),
    <<"----nova_test_", (integer_to_binary(Int))/binary>>.

multipart_part({field, Name, Value}, Boundary) ->
    [
        <<"--", Boundary/binary, "\r\n">>,
        <<"Content-Disposition: form-data; name=\"", Name/binary, "\"\r\n">>,
        <<"\r\n">>,
        Value,
        <<"\r\n">>
    ];
multipart_part({file, Name, Filename, ContentType, Data}, Boundary) ->
    [
        <<"--", Boundary/binary, "\r\n">>,
        <<"Content-Disposition: form-data; name=\"", Name/binary, "\"; filename=\"",
            Filename/binary, "\"\r\n">>,
        <<"Content-Type: ", ContentType/binary, "\r\n">>,
        <<"\r\n">>,
        Data,
        <<"\r\n">>
    ].
