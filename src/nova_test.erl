-module(nova_test).
-moduledoc """
HTTP client for integration testing Nova applications.

Start your Nova app, make HTTP requests, and assert on responses.

```erlang
Config = nova_test:start(my_app),
{ok, Resp} = nova_test:get("/api/users", Config),
?assertStatus(200, Resp),
?assertJson(#{<<"users">> := _}, Resp),
nova_test:stop(Config).
```

`start/1,2` fails when the application's HTTP listener is not running (for
example when the port is already in use), and stores the port the listener
actually bound, so `port => 0` in `cowboy_configuration` works.

Cookies can be carried by hand with `save_cookies/2`, or automatically with
`session/1`, which attaches a cookie jar to the Config. `csrf_token/1` reads a
CSRF token from a page, for form flows protected by `nova_csrf_plugin`.

JSON bodies are encoded and decoded with the `json_lib` module configured for
the Nova bootstrap application, or with OTP's `json` module when none is
configured.
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
    session/1,
    csrf_token/1,
    csrf_token/2,
    enable_logging/1,
    disable_logging/1,
    build_multipart_body/2
]).

-export_type([response/0, opts/0, multipart_field/0, form_fields/0, config/0]).

-ifdef(TEST).
-export([
    build_headers/1,
    build_request/3,
    cookie_header/1,
    merge_set_cookies/2,
    parse_set_cookie/1,
    maybe_log/5,
    generate_boundary/0,
    multipart_part/2
]).
-endif.

-type config() :: [{atom(), term()}].

-type method() :: delete | get | head | options | patch | post | put | trace.

-type response() :: #{
    status := integer(),
    headers := [{string(), string()}],
    body := binary()
}.

-type multipart_field() ::
    {field, Name :: binary(), Value :: binary()}
    | {file, Name :: binary(), Filename :: binary(), ContentType :: binary(), Data :: binary()}.

-type form_value() :: binary() | string() | atom() | integer().
-type form_fields() :: #{form_value() => form_value()} | [{form_value(), form_value()}].

-type opts() :: #{
    headers => [{binary(), binary()}],
    json => term(),
    form => form_fields(),
    body => binary(),
    content_type => string(),
    multipart => [multipart_field()]
}.

%% Lifecycle

-doc "Equivalent to `start(App, [])`.".
-spec start(App :: atom()) -> config().
start(App) ->
    start(App, []).

-doc """
Start `App` and its dependencies, and return `Config` with the port of the
running HTTP listener.

Raises `{nova_test_start_failed, #{app, reason}}` when `inets` or `App` cannot
be started, and `{nova_test_listener_not_running, #{app, reason, ...}}` when
the application started but its listener did not (Nova only logs
"Cowboy could not start" when the port is taken). In the second case the
applications started by this call are stopped again before raising.
""".
-spec start(App :: atom(), Config :: config()) -> config().
start(App, Config) ->
    _ = ensure_started(inets),
    Started = ensure_started(App),
    case listener_port(App) of
        {ok, Port} ->
            [{nova_test_port, Port}, {nova_test_app, App}, {nova_test_started, Started} | Config];
        {error, Reason} ->
            stop_apps(App, Started),
            erlang:error(
                {nova_test_listener_not_running, #{
                    app => App,
                    reason => Reason,
                    configured_port => configured_port()
                }}
            )
    end.

-doc """
Stop the application started by `start/1,2`, and the dependencies that call
started, so the next `start/1,2` gets a fresh listener.
""".
-spec stop(Config :: config()) -> ok.
stop(Config) ->
    App = proplists:get_value(nova_test_app, Config),
    Started =
        case proplists:get_value(nova_test_started, Config, []) of
            Apps when is_list(Apps) -> [A || A <- Apps, is_atom(A)];
            _ -> []
        end,
    case App of
        A when is_atom(A) -> stop_apps(A, Started);
        _ -> ok
    end.

%% HTTP methods

-spec get(Path :: string(), Config :: config()) -> {ok, response()} | {error, term()}.
get(Path, Config) ->
    request(get, Path, #{}, Config).

-spec get(Path :: string(), Opts :: opts(), Config :: config()) ->
    {ok, response()} | {error, term()}.
get(Path, Opts, Config) ->
    request(get, Path, Opts, Config).

-spec post(Path :: string(), Config :: config()) -> {ok, response()} | {error, term()}.
post(Path, Config) ->
    request(post, Path, #{}, Config).

-spec post(Path :: string(), Opts :: opts(), Config :: config()) ->
    {ok, response()} | {error, term()}.
post(Path, Opts, Config) ->
    request(post, Path, Opts, Config).

-spec put(Path :: string(), Opts :: opts(), Config :: config()) ->
    {ok, response()} | {error, term()}.
put(Path, Opts, Config) ->
    request(put, Path, Opts, Config).

-spec patch(Path :: string(), Opts :: opts(), Config :: config()) ->
    {ok, response()} | {error, term()}.
patch(Path, Opts, Config) ->
    request(patch, Path, Opts, Config).

-spec delete(Path :: string(), Config :: config()) -> {ok, response()} | {error, term()}.
delete(Path, Config) ->
    request(delete, Path, #{}, Config).

-spec delete(Path :: string(), Opts :: opts(), Config :: config()) ->
    {ok, response()} | {error, term()}.
delete(Path, Opts, Config) ->
    request(delete, Path, Opts, Config).

-doc """
Send a request to the application started by `start/1,2`.

When `Config` carries a session (see `session/1`), the session's cookies are
sent and the cookies set by the response are stored in it.
""".
-spec request(
    Method :: method(), Path :: string(), Opts :: opts(), Config :: config()
) ->
    {ok, response()} | {error, term()}.
request(Method, Path, Opts, Config) ->
    URL = "http://localhost:" ++ integer_to_list(port(Config)) ++ Path,
    Headers = build_headers(Opts) ++ cookie_header(Config),
    Request = build_request(URL, Headers, Opts),
    HttpOpts = [{autoredirect, false}],
    ReqOpts = [{body_format, binary}],
    case httpc:request(Method, Request, HttpOpts, ReqOpts) of
        {ok, {{_, Status, _}, RespHeaders, Body}} when
            is_integer(Status), is_list(RespHeaders), is_binary(Body)
        ->
            Response = #{
                status => Status,
                headers => httpc_headers(RespHeaders),
                body => Body
            },
            store_session_cookies(Response, Config),
            maybe_log(Method, Path, Opts, Response, Config),
            {ok, Response};
        {ok, Other} ->
            {error, {unexpected_response, Other}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Response accessors

-spec status(response()) -> integer().
status(#{status := Status}) -> Status.

-spec body(response()) -> binary().
body(#{body := Body}) -> Body.

-doc "Decode the response body as JSON.".
-spec json(response()) -> dynamic().
json(#{body := Body}) ->
    nova_test_json:decode(Body).

-spec headers(response()) -> [{string(), string()}].
headers(#{headers := Headers}) -> Headers.

-spec header(Name :: string(), response()) -> string() | undefined.
header(Name, #{headers := Headers}) ->
    LowerName = string:lowercase(Name),
    case [V || {K, V} <- Headers, string:lowercase(K) =:= LowerName] of
        [Value | _] -> Value;
        [] -> undefined
    end.

%% Cookie management

-doc """
Store the cookies set by `Response` in `Config`. Cookies the response
expires (`Max-Age=0` or an `Expires` date in the past) are removed.
""".
-spec save_cookies(response(), config()) -> config().
save_cookies(#{headers := Headers}, Config) ->
    put_cookies(merge_set_cookies(Headers, cookies(Config)), Config).

-doc "The cookies held by `Config`, from its session if it has one.".
-spec cookies(Config :: config()) -> #{binary() => binary()}.
cookies(Config) ->
    case session_jar(Config) of
        {ok, Jar} ->
            #{N => V || {N, V} <- ets:tab2list(Jar), is_binary(N), is_binary(V)};
        error ->
            case proplists:get_value(nova_test_cookies, Config, #{}) of
                Cookies when is_map(Cookies) ->
                    #{N => V || N := V <- Cookies, is_binary(N), is_binary(V)};
                _ ->
                    #{}
            end
    end.

-spec cookie(Name :: binary(), Config :: config()) -> binary() | undefined.
cookie(Name, Config) ->
    maps:get(Name, cookies(Config), undefined).

-spec set_cookie(Name :: binary(), Value :: binary(), Config :: config()) -> config().
set_cookie(Name, Value, Config) ->
    put_cookies((cookies(Config))#{Name => Value}, Config).

-spec clear_cookies(Config :: config()) -> config().
clear_cookies(Config) ->
    put_cookies(#{}, Config).

-doc """
Attach a new cookie jar to `Config`.

Every request made with the returned Config sends the jar's cookies and
stores the cookies set by the response, like a browser would, so session and
CSRF flows work without `save_cookies/2`. `cookies/1`, `cookie/2`,
`set_cookie/3` and `clear_cookies/1` read and write the jar. The jar starts
with the cookies `Config` already holds; call `session/1` once per simulated
client.

The jar is an ETS table owned by the calling process. Create it in the test
case (or `init_per_testcase/2`), not in `init_per_suite/1`, whose process
exits before the test cases run.

```erlang
Session = nova_test:session(Config),
{ok, Form} = nova_test:get("/login", Session),
{ok, Resp} = nova_test:post("/login", #{form => [
    {~"_csrf_token", nova_test:csrf_token(Form)},
    {~"email", ~"alice@example.com"},
    {~"password", ~"secret"}
]}, Session),
?assertStatus(302, Resp),
{ok, Page} = nova_test:get("/dashboard", Session).
```
""".
-spec session(Config :: config()) -> config().
session(Config) ->
    Cookies = cookies(Config),
    Jar = ets:new(nova_test_session, [set, public]),
    true = ets:insert(Jar, maps:to_list(Cookies)),
    put_config(nova_test_session, Jar, [KV || {K, _} = KV <- Config, K =/= nova_test_cookies]).

-doc "Equivalent to `csrf_token(~\"_csrf_token\", Source)`.".
-spec csrf_token(response() | binary()) -> binary() | undefined.
csrf_token(Source) ->
    csrf_token(~"_csrf_token", Source).

-doc """
Find a CSRF token in a response or an HTML binary.

Looks, in order, for the `value` of an `<input>` whose `name` is `Field`
(`nova_csrf_plugin`'s `field_name`, `~"_csrf_token"` by default), the
`content` of `<meta name="csrf-token">`, and, for a response, the
`x-csrf-token` header. Returns `undefined` when there is no token.
""".
-spec csrf_token(Field :: binary(), response() | binary()) -> binary() | undefined.
csrf_token(Field, Html) when is_binary(Html) ->
    case tag_attribute(Html, ~"input", ~"name", Field, ~"value") of
        undefined -> tag_attribute(Html, ~"meta", ~"name", ~"csrf-token", ~"content");
        Token -> Token
    end;
csrf_token(Field, #{body := Body} = Response) ->
    case csrf_token(Field, Body) of
        undefined ->
            case header("x-csrf-token", Response) of
                undefined -> undefined;
                Token -> list_to_binary(Token)
            end;
        Token ->
            Token
    end.

%% Logging

-spec enable_logging(Config :: config()) -> config().
enable_logging(Config) ->
    put_config(nova_test_logging, true, Config).

-spec disable_logging(Config :: config()) -> config().
disable_logging(Config) ->
    put_config(nova_test_logging, false, Config).

%% Multipart

-spec build_multipart_body([multipart_field()], Boundary :: binary()) -> iolist().
build_multipart_body(Fields, Boundary) ->
    Parts = [multipart_part(Field, Boundary) || Field <- Fields],
    [Parts, <<"--", Boundary/binary, "--\r\n">>].

%% Internal

-spec ensure_started(atom()) -> [atom()].
ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, Started} -> Started;
        {error, Reason} -> erlang:error({nova_test_start_failed, #{app => App, reason => Reason}})
    end.

-spec stop_apps(atom(), [atom()]) -> ok.
stop_apps(App, Started) ->
    _ = application:stop(App),
    stop_in_reverse([A || A <- Started, A =/= App]).

-spec stop_in_reverse([atom()]) -> ok.
stop_in_reverse([]) ->
    ok;
stop_in_reverse([App | Rest]) ->
    ok = stop_in_reverse(Rest),
    _ = application:stop(App),
    ok.

listener_port(App) ->
    case listener_ref(App) of
        {ok, Ref} ->
            try ranch:get_port(Ref) of
                Port when is_integer(Port) -> {ok, Port};
                _ -> {error, {no_tcp_port, Ref}}
            catch
                error:badarg -> {error, {listener_not_found, Ref}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Nova before 0.17 has a single listener, always named nova_listener.
listener_ref(App) ->
    case erlang:function_exported(nova_sup, get_started_applications, 0) of
        true -> started_listener(App, nova_sup:get_started_applications());
        false -> {ok, nova_listener}
    end.

started_listener(App, Started) ->
    Bootstrap = application:get_env(nova, bootstrap_application, undefined),
    Own = [L || #{app := A, listener := L} <- Started, A =:= App],
    Boot = [L || #{app := A, listener := L} <- Started, A =:= Bootstrap],
    case Own ++ Boot of
        [Ref | _] -> {ok, Ref};
        [] -> {error, no_listener_for_app}
    end.

configured_port() ->
    case application:get_env(nova, cowboy_configuration, #{}) of
        #{port := Port} -> Port;
        _ -> 8080
    end.

port(Config) ->
    case proplists:get_value(nova_test_port, Config) of
        Port when is_integer(Port) -> Port;
        _ -> erlang:error({nova_test_not_started, ~"call nova_test:start/1,2 first"})
    end.

build_headers(#{headers := Headers}) ->
    [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers];
build_headers(_) ->
    [].

build_request(URL, Headers, #{json := Json}) ->
    {URL, Headers, "application/json", nova_test_json:encode(Json)};
build_request(URL, Headers, #{form := Fields}) ->
    {URL, Headers, "application/x-www-form-urlencoded", nova_test_req:encode_qs(Fields)};
build_request(URL, Headers, #{multipart := Fields}) ->
    Boundary = generate_boundary(),
    Body = iolist_to_binary(build_multipart_body(Fields, Boundary)),
    CT = "multipart/form-data; boundary=" ++ binary_to_list(Boundary),
    {URL, Headers, CT, Body};
build_request(URL, Headers, #{body := Body, content_type := CT}) ->
    {URL, Headers, CT, Body};
build_request(URL, Headers, #{body := Body}) ->
    {URL, Headers, "text/plain", Body};
build_request(URL, Headers, _) ->
    {URL, Headers}.

%% httpc returns headers as strings.
-spec httpc_headers(dynamic()) -> [{string(), string()}].
httpc_headers(Headers) ->
    [{K, V} || {K, V} <- Headers].

-spec put_config(atom(), term(), config()) -> config().
put_config(Key, Value, Config) ->
    [{Key, Value} | [KV || {K, _} = KV <- Config, K =/= Key]].

-spec session_jar(config()) -> {ok, ets:tid()} | error.
session_jar(Config) ->
    case proplists:get_value(nova_test_session, Config) of
        undefined ->
            error;
        Jar ->
            live_jar(Jar)
    end.

%% The jar comes back out of the untyped Config proplist.
-spec live_jar(dynamic()) -> {ok, ets:tid()}.
live_jar(Jar) ->
    case ets:info(Jar, id) of
        undefined -> erlang:error({nova_test_session_closed, Jar});
        _ -> {ok, Jar}
    end.

put_cookies(Cookies, Config) ->
    case session_jar(Config) of
        {ok, Jar} ->
            true = ets:delete_all_objects(Jar),
            true = ets:insert(Jar, maps:to_list(Cookies)),
            Config;
        error ->
            put_config(nova_test_cookies, Cookies, Config)
    end.

store_session_cookies(Response, Config) ->
    case session_jar(Config) of
        {ok, _} ->
            _ = save_cookies(Response, Config),
            ok;
        error ->
            ok
    end.

cookie_header(Config) ->
    case lists:sort(maps:to_list(cookies(Config))) of
        [] ->
            [];
        Cookies ->
            Pairs = [
                [binary_to_list(N), "=", binary_to_list(V)]
             || {N, V} <- Cookies, is_binary(N), is_binary(V)
            ],
            [{"cookie", lists:flatten(lists:join("; ", Pairs))}]
    end.

merge_set_cookies([], Cookies) ->
    Cookies;
merge_set_cookies([{Key, Value} | Rest], Cookies) ->
    case string:lowercase(Key) of
        "set-cookie" ->
            case parse_set_cookie(Value) of
                {Name, _, expired} -> merge_set_cookies(Rest, maps:remove(Name, Cookies));
                {Name, Val, active} -> merge_set_cookies(Rest, Cookies#{Name => Val});
                error -> merge_set_cookies(Rest, Cookies)
            end;
        _ ->
            merge_set_cookies(Rest, Cookies)
    end.

parse_set_cookie(CookieStr) ->
    [NameValue | Attributes] = string:split(CookieStr, ";", all),
    case string:split(string:trim(NameValue), "=") of
        [Name, Value] when Name =/= "" ->
            {
                trimmed_binary(Name),
                trimmed_binary(Value),
                expiry([cookie_attribute(A) || A <- Attributes])
            };
        _ ->
            error
    end.

-spec trimmed_binary(unicode:chardata()) -> binary().
trimmed_binary(String) ->
    case unicode:characters_to_binary(string:trim(String)) of
        Binary when is_binary(Binary) -> Binary;
        _ -> erlang:error({invalid_cookie, String})
    end.

-spec cookie_attribute(unicode:chardata()) -> {binary(), binary()}.
cookie_attribute(Attribute) ->
    case string:split(string:trim(Attribute), "=") of
        [Name, Value] -> {trimmed_binary(string:lowercase(Name)), trimmed_binary(Value)};
        [Name | _] -> {trimmed_binary(string:lowercase(Name)), <<>>}
    end.

%% Max-Age wins over Expires (RFC 6265, section 5.3).
-spec expiry([{binary(), binary()}]) -> active | expired.
expiry(Attributes) ->
    case {[V || {~"max-age", V} <- Attributes], [V || {~"expires", V} <- Attributes]} of
        {[MaxAge | _], _} ->
            case string:to_integer(MaxAge) of
                {Seconds, <<>>} when is_integer(Seconds), Seconds =< 0 -> expired;
                _ -> active
            end;
        {[], [Date | _]} ->
            date_expiry(binary_to_list(Date));
        {[], []} ->
            active
    end.

date_expiry(Date) ->
    case httpd_util:convert_request_date(Date) of
        bad_date ->
            active;
        DateTime ->
            case DateTime =< calendar:universal_time() of
                true -> expired;
                false -> active
            end
    end.

tag_attribute(Html, Tag, KeyAttribute, KeyValue, ValueAttribute) ->
    case
        re:run(Html, <<"<", Tag/binary, "\\b[^>]*>">>, [global, caseless, {capture, first, binary}])
    of
        {match, Matches} ->
            find_tag([T || [T] <- Matches], KeyAttribute, KeyValue, ValueAttribute);
        nomatch ->
            undefined
    end.

find_tag([], _KeyAttribute, _KeyValue, _ValueAttribute) ->
    undefined;
find_tag([Tag | Rest], KeyAttribute, KeyValue, ValueAttribute) ->
    case {attribute(KeyAttribute, Tag), attribute(ValueAttribute, Tag)} of
        {{ok, KeyValue}, {ok, Value}} -> Value;
        _ -> find_tag(Rest, KeyAttribute, KeyValue, ValueAttribute)
    end.

attribute(Name, Tag) ->
    Pattern = <<"\\s", Name/binary, "\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s\"'>]+))">>,
    case re:run(Tag, Pattern, [caseless, {capture, all_but_first, binary}]) of
        {match, Groups} -> {ok, unescape_html(iolist_to_binary([G || G <- Groups, is_binary(G)]))};
        nomatch -> error
    end.

unescape_html(Value) ->
    unescape_html(Value, [
        {~"&quot;", ~"\""},
        {~"&#34;", ~"\""},
        {~"&#39;", ~"'"},
        {~"&#x27;", ~"'"},
        {~"&#x2F;", ~"/"},
        {~"&#47;", ~"/"},
        {~"&#x3D;", ~"="},
        {~"&#61;", ~"="},
        {~"&#43;", ~"+"},
        {~"&#x2B;", ~"+"},
        {~"&lt;", ~"<"},
        {~"&gt;", ~">"},
        {~"&amp;", ~"&"}
    ]).

unescape_html(Value, []) ->
    Value;
unescape_html(Value, [{Entity, Char} | Rest]) ->
    unescape_html(binary:replace(Value, Entity, Char, [global]), Rest).

maybe_log(Method, Path, Opts, #{status := Status, headers := RespHeaders, body := Body}, Config) ->
    case proplists:get_value(nova_test_logging, Config, false) of
        true ->
            ReqBody =
                case Opts of
                    #{json := Json} -> nova_test_json:encode(Json);
                    #{form := Fields} -> nova_test_req:encode_qs(Fields);
                    #{body := B} -> B;
                    _ -> <<>>
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
        _ ->
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
