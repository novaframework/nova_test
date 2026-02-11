-ifndef(NOVA_TEST_HRL).
-define(NOVA_TEST_HRL, true).

-include_lib("eunit/include/eunit.hrl").

%% HTTP response assertions (for integration tests)

-define(assertStatus(Expected, Resp),
    ?assertEqual(Expected, nova_test:status(Resp))).

-define(assertJson(Pattern, Resp),
    ?assertMatch(Pattern, nova_test:json(Resp))).

-define(assertBody(Expected, Resp),
    ?assertEqual(Expected, nova_test:body(Resp))).

-define(assertHeader(Name, Expected, Resp),
    ?assertEqual(Expected, nova_test:header(Name, Resp))).

%% Controller return assertions (for unit tests)

%% Matches {json, Body} where Body matches Pattern
-define(assertJsonResponse(Pattern, Result),
    ?assertMatch({json, Pattern}, Result)).

%% Matches {json, Status, _, Body} where Body matches Pattern
-define(assertJsonResponse(Status, Pattern, Result),
    ?assertMatch({json, Status, _, Pattern}, Result)).

%% Matches {status, Code}
-define(assertStatusResponse(Code, Result),
    ?assertMatch({status, Code}, Result)).

%% Matches {redirect, Target}
-define(assertRedirect(Target, Result),
    ?assertMatch({redirect, Target}, Result)).

%% Matches {ok, Variables} where Variables matches Pattern
-define(assertOkResponse(Pattern, Result),
    ?assertMatch({ok, Pattern}, Result)).

-endif.
