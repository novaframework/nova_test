# nova_test

Testing library for [Nova](https://github.com/novaframework/nova) framework applications.

Provides an HTTP client for integration tests and a mock request builder for unit tests.

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

## Usage

### Integration tests

Start the application under test and make HTTP requests:

```erlang
nova_test:start(),
{200, Headers, Body} = nova_test:get("/api/users", #{}).
```

### Assertion macros

Include the header for convenient assertions:

```erlang
-include_lib("nova_test/include/nova_test.hrl").

?assertStatus(200, Response),
?assertJson(#{<<"name">> => <<"alice">>}, Response),
?assertHeader(<<"content-type">>, <<"application/json">>, Response).
```

### Mock requests

Build mock Cowboy requests for unit-testing controllers without HTTP:

```erlang
Req = nova_test_request:new(<<"GET">>, <<"/users">>),
Req2 = nova_test_request:set_body(Req, <<"{\"name\":\"alice\"}">>),
Result = my_controller:handle(Req2).
```

## License

MIT
