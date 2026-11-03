-module(test_app_env).

-export([load/1, load/2]).

load(Port) ->
    load(Port, #{}).

load(Port, Extra) ->
    case
        application:load(
            {application, test_app, [
                {description, "Test app for nova_test"},
                {vsn, "0.0.1"},
                {mod, {test_app, []}},
                {applications, [kernel, stdlib, nova]},
                {modules, [
                    test_app,
                    test_sup,
                    test_app_router,
                    test_controller,
                    test_ws_handler,
                    test_ws_auth_handler
                ]}
            ]}
        )
    of
        ok -> ok;
        {error, {already_loaded, _}} -> ok
    end,
    application:set_env(nova, bootstrap_application, test_app),
    application:set_env(nova, cowboy_configuration, Extra#{port => Port}),
    application:set_env(nova, plugins, [
        {pre_request, nova_request_plugin, #{decode_json_body => true}}
    ]).
