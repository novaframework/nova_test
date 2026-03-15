-module(nova_test_ws).
-moduledoc """
WebSocket client for integration testing Nova WebSocket endpoints.

Uses `gun` to connect, send frames, and receive responses.

```erlang
{ok, Conn} = nova_test_ws:connect("/ws", Config),
ok = nova_test_ws:send_json(#{<<"action">> => <<"ping">>}, Conn),
{ok, Data} = nova_test_ws:recv_json(Conn),
ok = nova_test_ws:close(Conn).
```
""".

-export([
    connect/2,
    connect/3,
    send_text/2,
    send_json/2,
    recv/1,
    recv/2,
    recv_json/1,
    recv_json/2,
    close/1
]).

-export_type([conn/0, ws_opts/0]).

-ifdef(TEST).
-export([
    maybe_log_ws/3
]).
-endif.

-type conn() :: #{
    gun_pid := pid(),
    stream_ref := reference(),
    monitor_ref := reference(),
    config := [{atom(), term()}]
}.

-type ws_opts() :: #{
    headers => [{binary(), binary()}],
    protocols => [binary()]
}.

-spec connect(Path :: string(), Config :: [{atom(), term()}]) ->
    {ok, conn()} | {error, term()}.
connect(Path, Config) ->
    connect(Path, #{}, Config).

-spec connect(Path :: string(), Opts :: ws_opts(), Config :: [{atom(), term()}]) ->
    {ok, conn()} | {error, term()}.
connect(Path, Opts, Config) ->
    {ok, _} = application:ensure_all_started(gun),
    Port = proplists:get_value(nova_test_port, Config),
    {ok, GunPid} = gun:open("localhost", Port, #{protocols => [http]}),
    case gun:await_up(GunPid, 5000) of
        {ok, _Protocol} ->
            Headers = maps:get(headers, Opts, []),
            StreamRef = gun:ws_upgrade(GunPid, Path, Headers),
            MonRef = monitor(process, GunPid),
            receive
                {gun_upgrade, GunPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
                    maybe_log_ws("CONNECT", Path, Config),
                    {ok, #{
                        gun_pid => GunPid,
                        stream_ref => StreamRef,
                        monitor_ref => MonRef,
                        config => Config
                    }};
                {gun_response, GunPid, _, _, Status, _} ->
                    demonitor(MonRef, [flush]),
                    gun:close(GunPid),
                    {error, {upgrade_failed, Status}};
                {gun_error, GunPid, StreamRef, Reason} ->
                    demonitor(MonRef, [flush]),
                    gun:close(GunPid),
                    {error, Reason};
                {'DOWN', MonRef, process, GunPid, Reason} ->
                    {error, {gun_down, Reason}}
            after 5000 ->
                demonitor(MonRef, [flush]),
                gun:close(GunPid),
                {error, timeout}
            end;
        {error, Reason} ->
            gun:close(GunPid),
            {error, Reason}
    end.

-spec send_text(Text :: binary(), Conn :: conn()) -> ok.
send_text(Text, #{gun_pid := Pid, stream_ref := Ref, config := Config}) ->
    maybe_log_ws("SEND", Text, Config),
    gun:ws_send(Pid, Ref, {text, Text}).

-spec send_json(Json :: map(), Conn :: conn()) -> ok.
send_json(Json, Conn) ->
    JsonLib = json_lib(),
    Encoded = iolist_to_binary(JsonLib:encode(Json)),
    send_text(Encoded, Conn).

-spec recv(Conn :: conn()) -> {ok, binary()} | {error, timeout | term()}.
recv(Conn) ->
    recv(Conn, 5000).

-spec recv(Conn :: conn(), Timeout :: non_neg_integer()) ->
    {ok, binary()} | {error, timeout | term()}.
recv(#{gun_pid := Pid, stream_ref := Ref, monitor_ref := MonRef, config := Config}, Timeout) ->
    receive
        {gun_ws, Pid, Ref, {text, Data}} ->
            maybe_log_ws("RECV", Data, Config),
            {ok, Data};
        {gun_ws, Pid, Ref, {binary, Data}} ->
            maybe_log_ws("RECV (binary)", Data, Config),
            {ok, Data};
        {gun_ws, Pid, Ref, close} ->
            {error, closed};
        {gun_ws, Pid, Ref, {close, Code, _}} ->
            {error, {closed, Code}};
        {'DOWN', MonRef, process, Pid, Reason} ->
            {error, {gun_down, Reason}}
    after Timeout ->
        {error, timeout}
    end.

-spec recv_json(Conn :: conn()) -> {ok, map()} | {error, term()}.
recv_json(Conn) ->
    recv_json(Conn, 5000).

-spec recv_json(Conn :: conn(), Timeout :: non_neg_integer()) -> {ok, map()} | {error, term()}.
recv_json(Conn, Timeout) ->
    case recv(Conn, Timeout) of
        {ok, Data} ->
            JsonLib = json_lib(),
            {ok, Decoded} = JsonLib:decode(Data),
            {ok, Decoded};
        {error, Reason} ->
            {error, Reason}
    end.

-spec close(Conn :: conn()) -> ok.
close(#{gun_pid := Pid, monitor_ref := MonRef, config := Config}) ->
    maybe_log_ws("CLOSE", "", Config),
    demonitor(MonRef, [flush]),
    gun:close(Pid),
    ok.

%% Internal

json_lib() ->
    try
        nova:get_env(json_lib, thoas)
    catch
        _:_ -> thoas
    end.

maybe_log_ws(Action, Detail, Config) ->
    case proplists:get_value(nova_test_logging, Config, false) of
        true ->
            ct:pal("~n=== nova_test_ws ~s ===~n~ts~n", [Action, Detail]);
        false ->
            ok
    end.
