# nova_test

Testing library for [Nova](https://github.com/novaframework/nova) framework applications.

Provides an HTTP client for integration tests, a WebSocket client, a mock request builder
for unit tests, cookie sessions with CSRF helpers, multipart uploads, and request/response
logging.

## Installation

Add `nova_test` to your test dependencies in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {deps, [
            {nova_test, "~> 0.2"}
        ]}
    ]}
]}.
```

## Integration Tests

Start the application under test and make HTTP requests:

```erlang
-include_lib("nova_test/include/nova_test.hrl").

init_per_suite(Config) ->
    nova_test:start(my_app, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

get_users_test(Config) ->
    {ok, Resp} = nova_test:get("/api/users", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"users">> := _}, Resp).

create_user_test(Config) ->
    {ok, Resp} = nova_test:post("/api/users", #{json => #{<<"name">> => <<"alice">>}}, Config),
    ?assertStatus(201, Resp).
```

`nova_test:start/1,2` starts the application and its dependencies and reads the port
the HTTP listener actually bound from Ranch, so `#{port => 0}` in `cowboy_configuration`
works. If the listener is not running (Nova only logs "Cowboy could not start" when the
port is taken), `start` stops what it started and raises
`{nova_test_listener_not_running, #{app := App, reason := Reason, configured_port := Port}}`
instead of letting the tests talk to whatever owns the port. `nova_test:stop/1` stops the
application and the dependencies `start` started, so the next suite gets a fresh listener.

### Request Options

```erlang
#{
    headers => [{<<"authorization">>, <<"Bearer token">>}],
    json => #{<<"key">> => <<"value">>},
    form => [{<<"email">>, <<"alice@example.com">>}],   %% application/x-www-form-urlencoded
    body => <<"raw bytes">>,
    content_type => "text/plain",
    multipart => [
        {field, <<"name">>, <<"alice">>},
        {file, <<"avatar">>, <<"photo.png">>, <<"image/png">>, FileData}
    ]
}
```

### JSON

`json` request bodies, `nova_test:json/1`, `nova_test_req:with_json/2` and the WebSocket
JSON helpers use the `json_lib` configured for the Nova bootstrap application (the same
key Nova reads). When none is configured, for example in EUnit without a running app,
they use OTP's `json` module. A `json_lib` module may return `{ok, Term}` or the term
itself from `decode/1`.

### Cookie Management

Carry cookies by hand with `save_cookies/2`:

```erlang
{ok, Resp} = nova_test:post("/login", #{json => Credentials}, Config),
Config2 = nova_test:save_cookies(Resp, Config),
%% Requests made with Config2 send the saved cookies
{ok, Resp2} = nova_test:get("/dashboard", Config2),
?assertStatus(200, Resp2).
```

Cookies the response expires (`Max-Age=0`, or an `Expires` date in the past) are
removed.

### Sessions and CSRF

`nova_test:session/1` attaches a cookie jar to the Config. Every request made with it
sends the jar's cookies and stores the cookies the response sets, like a browser, so
Nova sessions and `nova_csrf_plugin` form flows work without calling `save_cookies/2`.
`nova_test:csrf_token/1` reads the token from the page's hidden `_csrf_token` input
(or `<meta name="csrf-token">`, or an `x-csrf-token` response header);
`csrf_token/2` takes another field name.

```erlang
log_in_test(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    {ok, Resp} = nova_test:post("/login", #{form => [
        {<<"_csrf_token">>, nova_test:csrf_token(Form)},
        {<<"email">>, <<"alice@example.com">>},
        {<<"password">>, <<"secret">>}
    ]}, Session),
    ?assertStatus(302, Resp),
    {ok, Page} = nova_test:get("/dashboard", Session),
    ?assertStatus(200, Page).
```

Call `session/1` once per simulated client. The jar is an ETS table owned by the calling
process: create it in the test case or `init_per_testcase/2`, not in `init_per_suite/1`.

### Debugging

Enable request/response logging to see what's happening in CT logs:

```erlang
Config2 = nova_test:enable_logging(Config),
{ok, _} = nova_test:get("/api/users", Config2).
%% Prints method, path, headers, body, status to ct:pal
```

## WebSocket Tests

```erlang
{ok, Conn} = nova_test_ws:connect("/ws", Config),
ok = nova_test_ws:send_json(#{<<"action">> => <<"ping">>}, Conn),
?assertWsRecvJson(#{<<"action">> := <<"pong">>}, Conn),
ok = nova_test_ws:close(Conn).
```

## Unit Tests (Mock Requests)

Build mock Cowboy requests to test controllers directly without HTTP. The builders fill
the keys Nova's `nova_request_plugin` would, so controllers see the same request map:

```erlang
Req = nova_test_req:new(post, "/api/users"),
Req1 = nova_test_req:with_json(#{<<"name">> => <<"alice">>}, Req),
Req2 = nova_test_req:with_auth_data(#{role => admin}, Req1),
Result = my_controller:create(Req2),
?assertJsonResponse(201, #{<<"id">> := _}, Result).
```

### Available Builders

- `new/2` - create a request with method and path
- `with_json/2` - set JSON body and content-type, and `json` (as `decode_json_body` does)
- `with_form/2` - set a form-urlencoded body and `params` (as `read_urlencoded_body` does)
- `with_body/2` - set raw body (not parsed)
- `with_header/3` - add a header
- `with_bindings/2` - set path parameter bindings
- `with_query/2` - set the query string and `parsed_qs` (as `parse_qs => true` does);
  pass a list of pairs to repeat a key
- `with_auth_data/2` - set auth context (for security middleware)
- `with_peer/2` - set source IP/port
- `with_multipart/2` - set multipart form-data body
- `with_cookies/2` - set cookie header

## Assertion Macros

Include `-include_lib("nova_test/include/nova_test.hrl").` for all macros.

### Response Assertions (Integration Tests)

| Macro | Description |
|-------|-------------|
| `?assertStatus(Code, Resp)` | Assert HTTP status code |
| `?assertJson(Pattern, Resp)` | Pattern match on decoded JSON body |
| `?assertBody(Expected, Resp)` | Assert exact body match |
| `?assertHeader(Name, Value, Resp)` | Assert response header value |
| `?assertCookie(Name, Value, Config)` | Assert cookie value in Config |

### WebSocket Assertions

| Macro | Description |
|-------|-------------|
| `?assertWsRecv(Pattern, Conn)` | Receive and pattern match text frame |
| `?assertWsRecvJson(Pattern, Conn)` | Receive and pattern match JSON frame |

### Controller Return Assertions (Unit Tests)

| Macro | Description |
|-------|-------------|
| `?assertJsonResponse(Pattern, Result)` | Match `{json, Body}` |
| `?assertJsonResponse(Status, Pattern, Result)` | Match `{json, Status, _, Body}` |
| `?assertStatusResponse(Code, Result)` | Match `{status, Code}` |
| `?assertRedirect(Target, Result)` | Match `{redirect, Target}` |
| `?assertOkResponse(Pattern, Result)` | Match `{ok, Variables}` |

### Test Generators

Every macro has a `?_assert...` variant that returns an EUnit test object instead of
running the assertion, for use in `_test_()` generators:

```erlang
user_test_() ->
    {setup, fun start/0, fun stop/1, fun(Config) ->
        {ok, Resp} = nova_test:get("/api/users/1", Config),
        [?_assertStatus(200, Resp), ?_assertJson(#{<<"id">> := 1}, Resp)]
    end}.
```

## License

MIT
