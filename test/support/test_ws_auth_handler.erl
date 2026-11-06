-module(test_ws_auth_handler).
-behaviour(nova_websocket).

-export([
    init/1,
    websocket_handle/2,
    websocket_info/2
]).

init(#{req := Req} = State) ->
    case nova_session:get(Req, ~"user") of
        {ok, User} -> {ok, State#{user => User}};
        {error, _} -> {error, unauthenticated}
    end.

websocket_handle({text, ~"whoami"}, #{user := User} = State) ->
    {reply, {text, User}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
