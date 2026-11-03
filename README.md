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
            {nova_test, "~> 0.3"}
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

Paths must start with `/`. Any other path raises
`{nova_test_invalid_path, #{path := Path, reason := Reason}}`, because a path such as
`"@other-host/x"` would send the request, with its cookies and headers, to another host.
`nova_test_ws:connect/2,3` checks its path the same way.

`nova_test:start/1,2` starts the application and its dependencies and reads the port
the HTTP listener actually bound from Ranch, so `#{port => 0}` in `cowboy_configuration`
works. If the listener is not running (Nova only logs "Cowboy could not start" when the
port is taken), `start` stops what it started and raises
`{nova_test_listener_not_running, #{app := App, reason := Reason, configured_port := Port}}`
instead of letting the tests talk to whatever owns the port. An application that was
already running before the call is left running.

When the application has several listeners, `start` uses the one bound to the port
`cowboy_configuration` asks for (`ssl_port` with `use_ssl => true`, otherwise `port`).

Only plain HTTP listeners are supported. With `use_ssl => true` in
`cowboy_configuration`, `start` stops what it started and raises
`{nova_test_tls_not_supported, #{app := App, port := Port}}`; test against a plain HTTP
listener (for example a test profile without `use_ssl`).

Call `nova_test:stop/1` last: it stops the application and the dependencies `start`
started (nova, cowboy, ranch and any others), so the next suite gets a fresh listener. A
dependency stays running while another running application still lists it in its
`applications`, so `start(a)`, `start(b)`, `stop(a)` keeps the listener `b` uses. The
next `stop` stops it once nothing needs it, in either order.

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
JSON helpers go through `nova_json` when the Nova in use has it. Otherwise they read
`json_lib` in the same order Nova does: the bootstrap application's environment, then
`nova`'s. When neither sets it, for example in EUnit without a running app, they use OTP's
`json` module.

Nova itself (0.18.x) defaults to thoas when `json_lib` is unset, so a running app and
nova_test can then use different libraries. Set `json_lib` explicitly if that matters.
Decode failures raise `{json_decode_failed, Reason}`. A `json_lib` module may return `{ok, Term}` or the term
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

`nova_test:session/1` attaches a cookie jar to the Config. Every request made with it,
including `nova_test_ws:connect/2,3`, sends the jar's cookies that match the request path
and stores the cookies the response sets, like a browser. Nova sessions and
`nova_csrf_plugin` form flows then work without calling `save_cookies/2`. The jar keys
cookies on name and `Path` (RFC 6265 path matching and default path); `Domain`, `Secure`
and `HttpOnly` are ignored, since every request goes to the application under test.
`nova_test:cookie_header/2` returns the `cookie` header for a path, for use with another
client. A `cookie` header you pass in `headers` is merged with the jar's into one header,
your pairs first.

To assert cookie attributes, read them from the response with `nova_test:set_cookies/1`.
It returns every `Set-Cookie` header (`nova_test:header/2` returns only the first) as
`#{name, value, attributes}`, with lowercased attribute names and `<<>>` for flags:

```erlang
session_cookie_flags_test(Config) ->
    Session = nova_test:session(Config),
    {ok, Resp} = nova_test:get("/login", Session),
    [#{attributes := Attrs}] =
        [C || #{name := <<"session_id">>} = C <- nova_test:set_cookies(Resp)],
    ?assert(lists:keymember(<<"httponly">>, 1, Attrs)),
    ?assert(lists:keymember(<<"secure">>, 1, Attrs)),
    ?assertEqual({<<"samesite">>, <<"Lax">>}, lists:keyfind(<<"samesite">>, 1, Attrs)).
```

`nova_test:csrf_token/1` follows `nova_csrf_plugin`'s conventions: it reads the
`value` of the page's `<input name="_csrf_token">`, and otherwise the `x-csrf-token`
response header. The plugin only reads that header from requests and never sets it on
responses, so the fallback finds a token only when your application sets the header
itself. If you changed the plugin's `field_name` or `header_name`, pass them:
`nova_test:csrf_token(#{field => <<"csrf">>, header => "x-xsrf-token"}, Resp)`.

`csrf_token` returns `undefined` when the page has no token. A `form` value of `undefined`
raises `{nova_test_undefined_form_value, #{field := Name, reason := Reason}}` instead of
posting the string `"undefined"`, so a missing token fails the test where it happens.

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

For the negative case, post from a session that holds a token. `nova_csrf_plugin` also
answers 403 when there is no session at all, so a 403 from a fresh client proves nothing
about the token check:

```erlang
csrf_rejected_test(Config) ->
    Session = nova_test:session(Config),
    {ok, Form} = nova_test:get("/login", Session),
    ?assert(is_binary(nova_test:csrf_token(Form))),
    {ok, Missing} = nova_test:post("/login", #{form => [{<<"email">>, <<"a@b">>}]}, Session),
    ?assertStatus(403, Missing),
    {ok, Forged} = nova_test:post("/login", #{form => [
        {<<"_csrf_token">>, <<"forged">>},
        {<<"email">>, <<"a@b">>}
    ]}, Session),
    ?assertStatus(403, Forged).
```

Call `session/1` once per simulated client and use each jar from one process at a time.
The jar is an ETS table owned by the calling process: create it in the test case or
`init_per_testcase/2`, not in `init_per_suite/1` or `init_per_group/2`, whose processes
exit before the test cases run.

### Debugging

Enable request/response logging to see what's happening in CT logs:

```erlang
Config2 = nova_test:enable_logging(Config),
{ok, _} = nova_test:get("/api/users", Config2).
%% Prints method, path, headers, body, status to ct:pal
```

The values of the `authorization`, `cookie`, `set-cookie` and `x-csrf-token` headers
and of the `password`, `password_confirmation` and `_csrf_token` form fields are logged
as `[redacted]`. JSON and raw bodies are logged as sent. The list is fixed: a custom CSRF
`field` or `header` passed to `csrf_token/2` is not redacted.

## WebSocket Tests

```erlang
{ok, Conn} = nova_test_ws:connect("/ws", Config),
ok = nova_test_ws:send_json(#{<<"action">> => <<"ping">>}, Conn),
?assertWsRecvJson(#{<<"action">> := <<"pong">>}, Conn),
ok = nova_test_ws:close(Conn).
```

The upgrade request carries the cookies the Config holds for that path, so a WebSocket
behind a session login works with `Session = nova_test:session(Config)`.

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
- `with_json/2` - set JSON body and content-type, and `json` decoded from that body (as
  `decode_json_body` does, so keys are binaries)
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
