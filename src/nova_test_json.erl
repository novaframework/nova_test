-module(nova_test_json).
-moduledoc false.

-export([encode/1, decode/1, json_lib/0]).

-spec encode(term()) -> binary().
encode(Term) ->
    Lib = json_lib(),
    case Lib:encode(Term) of
        {ok, Encoded} -> iolist_to_binary(Encoded);
        Encoded -> iolist_to_binary(Encoded)
    end.

-spec decode(iodata()) -> dynamic().
decode(Data) ->
    Lib = json_lib(),
    case Lib:decode(iolist_to_binary(Data)) of
        {ok, Decoded} -> Decoded;
        {error, Reason} -> erlang:error({json_decode_failed, Lib, Reason});
        Decoded -> Decoded
    end.

-spec json_lib() -> module().
json_lib() ->
    case nova:get_env(json_lib, json) of
        Lib when is_atom(Lib), Lib =/= undefined -> Lib;
        _ -> json
    end.
