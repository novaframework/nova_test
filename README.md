# nova_test

Testing library for [Nova](https://github.com/novaframework/nova) framework applications.

Provides an HTTP client for integration tests, a WebSocket client, a mock request builder
for unit tests, cookie management, multipart uploads, and request/response logging.

## Installation

Add `nova_test` to your test dependencies in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {deps, [
            {nova_test, "~> 0.1"}
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

### Request Options

```erlang
#{
    headers => [{<<"authorization">>, <<"Bearer token">>}],
    json => #{<<"key">> => <<"value">>},
    body => <<"raw bytes">>,
    content_type => "text/plain",
    multipart => [
        {field, <<"name">>, <<"alice">>},
        {file, <<"avatar">>, <<"photo.png">>, <<"image/png">>, FileData}
    ]
}
```

### Cookie Management

```erlang
{ok, Resp} = nova_test:post("/login", #{json => Credentials}, Config),
Config2 = nova_test:save_cookies(Resp, Config),
%% Subsequent requests carry the session cookie automatically
{ok, Resp2} = nova_test:get("/dashboard", Config2),
?assertStatus(200, Resp2).
```

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

Build mock Cowboy requests to test controllers directly without HTTP:

```erlang
Req = nova_test_req:new(post, "/api/users"),
Req1 = nova_test_req:with_json(#{<<"name">> => <<"alice">>}, Req),
Req2 = nova_test_req:with_auth_data(#{role => admin}, Req1),
Result = my_controller:create(Req2),
?assertJsonResponse(201, #{<<"id">> := _}, Result).
```

### Available Builders

- `new/2` - create a request with method and path
- `with_json/2` - set JSON body and content-type
- `with_body/2` - set raw body
- `with_header/3` - add a header
- `with_bindings/2` - set path parameter bindings
- `with_query/2` - set query string parameters
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

## License

MIT
