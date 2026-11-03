-module(nova_test_json).
-moduledoc false.

-export([encode/1, decode/1, json_lib/0]).

-spec encode(term()) -> binary().
encode(Term) ->
    Encoded =
        case nova_json_module() of
            {ok, NovaJson} -> NovaJson:encode(Term);
            error -> (json_lib()):encode(Term)
        end,
    case Encoded of
        {ok, IoData} -> iolist_to_binary(IoData);
        IoData -> iolist_to_binary(IoData)
    end.

-spec decode(iodata()) -> dynamic().
decode(Data) ->
    Json = iolist_to_binary(Data),
    Result =
        try
            case nova_json_module() of
                {ok, NovaJson} -> NovaJson:decode(Json);
                error -> (json_lib()):decode(Json)
            end
        catch
            Class:Reason -> {error, {Class, Reason}}
        end,
    case Result of
        {ok, Decoded} -> Decoded;
        {error, Error} -> erlang:error({json_decode_failed, Error});
        Decoded -> Decoded
    end.

-doc """
The `json_lib` Nova would use when `nova_json` is not available: the
bootstrap application's environment, then `nova`'s, then OTP `json`.
""".
-spec json_lib() -> module().
json_lib() ->
    case nova:get_env(json_lib, undefined) of
        Lib when is_atom(Lib), Lib =/= undefined ->
            Lib;
        _ ->
            case application:get_env(nova, json_lib) of
                {ok, Lib} when is_atom(Lib), Lib =/= undefined -> Lib;
                _ -> json
            end
    end.

%% nova_json (Nova 0.19+) resolves json_lib itself; delegate when it exists.
-spec nova_json_module() -> {ok, module()} | error.
nova_json_module() ->
    case code:ensure_loaded(nova_json) of
        {module, nova_json} ->
            case
                erlang:function_exported(nova_json, encode, 1) andalso
                    erlang:function_exported(nova_json, decode, 1)
            of
                true -> {ok, nova_json};
                false -> error
            end;
        _ ->
            error
    end.
