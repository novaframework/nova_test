-module(nova_test).

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
    header/2
]).

-type response() :: #{
    status := integer(),
    headers := [{string(), string()}],
    body := binary()
}.

-type opts() :: #{
    headers => [{binary(), binary()}],
    json => map(),
    body => binary(),
    content_type => string()
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

-spec get(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
get(Path, Opts, Config) ->
    request(get, Path, Opts, Config).

-spec post(Path :: string(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
post(Path, Config) ->
    request(post, Path, #{}, Config).

-spec post(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
post(Path, Opts, Config) ->
    request(post, Path, Opts, Config).

-spec put(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
put(Path, Opts, Config) ->
    request(put, Path, Opts, Config).

-spec patch(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
patch(Path, Opts, Config) ->
    request(patch, Path, Opts, Config).

-spec delete(Path :: string(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
delete(Path, Config) ->
    request(delete, Path, #{}, Config).

-spec delete(Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) -> {ok, response()} | {error, term()}.
delete(Path, Opts, Config) ->
    request(delete, Path, Opts, Config).

-spec request(Method :: atom(), Path :: string(), Opts :: opts(), Config :: [{atom(), term()}]) ->
    {ok, response()} | {error, term()}.
request(Method, Path, Opts, Config) ->
    Port = proplists:get_value(nova_test_port, Config),
    URL = "http://localhost:" ++ integer_to_list(Port) ++ Path,
    Headers = build_headers(Opts),
    HttpMethod = Method,
    Request = build_request(URL, Headers, Opts),
    HttpOpts = [{autoredirect, false}],
    ReqOpts = [{body_format, binary}],
    case httpc:request(HttpMethod, Request, HttpOpts, ReqOpts) of
        {ok, {{_, Status, _}, RespHeaders, Body}} ->
            {ok, #{
                status => Status,
                headers => RespHeaders,
                body => Body
            }};
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
    BodyStr = case Body of
        B when is_binary(B) -> binary_to_list(B);
        B when is_list(B) -> B
    end,
    {URL, Headers, "application/json", BodyStr};
build_request(URL, Headers, #{body := Body, content_type := CT}) ->
    BodyStr = binary_to_list(Body),
    {URL, Headers, CT, BodyStr};
build_request(URL, Headers, #{body := Body}) ->
    BodyStr = binary_to_list(Body),
    {URL, Headers, "text/plain", BodyStr};
build_request(URL, Headers, _) ->
    {URL, Headers}.

json_lib() ->
    try nova:get_env(json_lib, thoas)
    catch _:_ -> thoas
    end.
