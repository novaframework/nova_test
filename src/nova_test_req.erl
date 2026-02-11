-module(nova_test_req).

-export([
    new/2,
    with_bindings/2,
    with_json/2,
    with_header/3,
    with_auth_data/2,
    with_query/2,
    with_body/2,
    with_peer/2
]).

-spec new(Method :: atom(), Path :: string() | binary()) -> cowboy_req:req().
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

-spec with_bindings(Bindings :: map(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_bindings(Bindings, Req) ->
    Req#{bindings => Bindings}.

-spec with_json(Json :: map(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_json(Json, Req) ->
    JsonLib = json_lib(),
    Body = JsonLib:encode(Json),
    Headers = maps:get(headers, Req, #{}),
    Req#{
        json => Json,
        body => Body,
        headers => Headers#{<<"content-type">> => <<"application/json">>}
    }.

-spec with_header(Name :: binary(), Value :: binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_header(Name, Value, Req) ->
    Headers = maps:get(headers, Req, #{}),
    Req#{headers => Headers#{Name => Value}}.

-spec with_auth_data(AuthData :: term(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_auth_data(AuthData, Req) ->
    Req#{auth_data => AuthData}.

-spec with_query(Params :: map(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_query(Params, Req) ->
    QS = maps:fold(fun(K, V, Acc) ->
        KB = to_binary(K),
        VB = to_binary(V),
        case Acc of
            <<>> -> <<KB/binary, "=", VB/binary>>;
            _ -> <<Acc/binary, "&", KB/binary, "=", VB/binary>>
        end
    end, <<>>, Params),
    Req#{qs => QS}.

-spec with_body(Body :: binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
with_body(Body, Req) ->
    Req#{body => Body}.

-spec with_peer(Peer :: {inet:ip_address(), inet:port_number()}, Req :: cowboy_req:req()) -> cowboy_req:req().
with_peer(Peer, Req) ->
    Req#{peer => Peer}.

%% Internal

method_to_binary(get) -> <<"GET">>;
method_to_binary(post) -> <<"POST">>;
method_to_binary(put) -> <<"PUT">>;
method_to_binary(patch) -> <<"PATCH">>;
method_to_binary(delete) -> <<"DELETE">>;
method_to_binary(head) -> <<"HEAD">>;
method_to_binary(options) -> <<"OPTIONS">>;
method_to_binary(Method) when is_atom(Method) ->
    list_to_binary(string:uppercase(atom_to_list(Method))).

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V).

json_lib() ->
    try nova:get_env(json_lib, thoas)
    catch _:_ -> thoas
    end.
