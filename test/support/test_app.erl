-module(test_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test_sup:start_link().

%% Records which dependency supervisors were still up when the app stopped,
%% so a test can check the stop order without calling the busy application
%% controller.
stop(_State) ->
    persistent_term:put(
        {test_app, running_at_stop},
        [Sup || Sup <- [nova_sup, cowboy_sup, ranch_sup], whereis(Sup) =/= undefined]
    ),
    ok.
