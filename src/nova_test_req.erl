-module(nova_test_req).
-moduledoc """
Mock Cowboy request builder for unit testing Nova controllers without HTTP.

Builds request maps compatible with Nova's controller interface,
enabling fast isolated tests that call controller functions directly.

```erlang
Req = nova_test_req:new(post, "/api/users"),
Req1 = nova_test_req:with_json(#{<<"name">> => <<"alice">>}, Req),
Result = my_controller:create(Req1),
?assertJsonResponse(201, #{<<"id">> := _}, Result).
```
""".

-export([
    new/2,
    with_bindings/2,
    with_json/2,
    with_header/3,
    with_auth_data/2,
    with_query/2,
    with_body/2,
    with_form/2,
    with_peer/2,
    with_multipart/2,
    with_cookies/2,
    encode_qs/1
]).

-export_type([req/0]).

-doc """
A mock request: a map with the keys a Nova controller reads from a
`cowboy_req:req()`. It is not a full `cowboy_req:req()`: there is no
connection, so `pid`, `streamid` and the other connection keys are missing.
""".
-type req() :: map().

-spec new(Method :: atom(), Path :: string() | binary()) -> req().
new(Method, Path) when is_list(Path) ->
    new(Method, list_to_binary(Path));
new(Method, Path) ->
    MethodBin = method_to_binary(Method),
    #{
        method => MethodBin,
        path => Path,
        host => <<"localhost">>,
        port => 8080,
        scheme => <<"http">>,
        qs => <<>>,
        version => 'HTTP/1.1',
        headers => #{},
        bindings => #{},
        peer => {{127, 0, 0, 1}, 12345}
    }.

-spec with_bindings(Bindings :: map(), Req :: req()) -> req().
with_bindings(Bindings, Req) ->
    Req#{bindings => Bindings}.

-doc """
Set a JSON body. `body` holds `Json` encoded with the configured `json_lib`
(thoas, Nova's default, when none is configured), and `json` holds that body decoded
again, the way `nova_request_plugin` with `decode_json_body` leaves it: keys
are binaries even when `Json` used atoms.
""".
-spec with_json(Json :: term(), Req :: req()) -> req().
with_json(Json, Req) ->
    Headers = maps:get(headers, Req, #{}),
    Body = nova_test_json:encode(Json),
    Req#{
        json => nova_test_json:decode(Body),
        body => Body,
        headers => Headers#{~"content-type" => ~"application/json"}
    }.

-spec with_header(Name :: binary(), Value :: binary(), Req :: req()) -> req().
with_header(Name, Value, Req) ->
    Headers = maps:get(headers, Req, #{}),
    Req#{headers => Headers#{Name => Value}}.

-spec with_auth_data(AuthData :: term(), Req :: req()) -> req().
with_auth_data(AuthData, Req) ->
    Req#{auth_data => AuthData}.

-doc """
Set the query string. `qs` holds the encoded string and `parsed_qs` holds it
parsed the way `nova_request_plugin` with `parse_qs => true` does: a map with
binary keys and values, where the last of repeated keys wins. Pass a list of
pairs to repeat a key. For `parse_qs => list`, use `cow_qs:parse_qs(maps:get(qs, Req))`.
""".
-spec with_query(Params :: nova_test:form_fields(), Req :: req()) ->
    req().
with_query(Params, Req) ->
    QS = encode_qs(Params),
    Req#{qs => QS, parsed_qs => maps:from_list(cow_qs:parse_qs(QS))}.

-doc "Set a raw body. The body is not parsed; see `with_json/2` and `with_form/2`.".
-spec with_body(Body :: binary(), Req :: req()) -> req().
with_body(Body, Req) ->
    Req#{body => Body}.

-doc """
Set an `application/x-www-form-urlencoded` body. `params` holds the fields
parsed the way `nova_request_plugin` with `read_urlencoded_body => true`
does: a map with binary keys and values.
""".
-spec with_form(Fields :: nova_test:form_fields(), Req :: req()) ->
    req().
with_form(Fields, Req) ->
    Body = encode_qs(Fields),
    Headers = maps:get(headers, Req, #{}),
    Req#{
        body => Body,
        params => maps:from_list(cow_qs:parse_qs(Body)),
        headers => Headers#{~"content-type" => ~"application/x-www-form-urlencoded"}
    }.

-spec with_peer(Peer :: {inet:ip_address(), inet:port_number()}, Req :: req()) ->
    req().
with_peer(Peer, Req) ->
    Req#{peer => Peer}.

-spec with_multipart(Fields :: [nova_test:multipart_field()], Req :: req()) ->
    req().
with_multipart(Fields, Req) ->
    Boundary = <<"----nova_test_boundary">>,
    Body = iolist_to_binary(nova_test:build_multipart_body(Fields, Boundary)),
    Headers = maps:get(headers, Req, #{}),
    CT = <<"multipart/form-data; boundary=", Boundary/binary>>,
    Req#{
        body => Body,
        headers => Headers#{<<"content-type">> => CT},
        multipart => Fields
    }.

-spec with_cookies(Cookies :: map(), Req :: req()) -> req().
with_cookies(Cookies, Req) ->
    CookieStr = maps:fold(
        fun(Name, Value, Acc) ->
            Pair = <<Name/binary, "=", Value/binary>>,
            case Acc of
                <<>> -> Pair;
                _ -> <<Acc/binary, "; ", Pair/binary>>
            end
        end,
        <<>>,
        Cookies
    ),
    Headers = maps:get(headers, Req, #{}),
    Req#{headers => Headers#{<<"cookie">> => CookieStr}}.

-doc false.
-spec encode_qs(nova_test:form_fields()) -> binary().
encode_qs(Fields) when is_map(Fields) ->
    encode_qs(maps:to_list(Fields));
encode_qs(Fields) ->
    cow_qs:qs([{to_binary(K), field_value(K, V)} || {K, V} <- Fields]).

%% `undefined` is what nova_test:csrf_token/1 returns when it finds no token.
field_value(Name, undefined) ->
    erlang:error(
        {nova_test_undefined_form_value, #{
            field => Name,
            reason => ~"form value is undefined, for example a csrf_token/1 that found no token"
        }}
    );
field_value(_Name, Value) ->
    to_binary(Value).

%% Internal

method_to_binary(get) ->
    <<"GET">>;
method_to_binary(post) ->
    <<"POST">>;
method_to_binary(put) ->
    <<"PUT">>;
method_to_binary(patch) ->
    <<"PATCH">>;
method_to_binary(delete) ->
    <<"DELETE">>;
method_to_binary(head) ->
    <<"HEAD">>;
method_to_binary(options) ->
    <<"OPTIONS">>;
method_to_binary(Method) when is_atom(Method) ->
    list_to_binary(string:uppercase(atom_to_list(Method))).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) ->
    case unicode:characters_to_binary(V) of
        B when is_binary(B) -> B;
        _ -> erlang:error({invalid_string, V})
    end;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V).
