-module(nova_test_json_error_lib).

-export([encode/1, decode/1]).

encode(_Term) ->
    <<>>.

decode(_Data) ->
    {error, bad}.
