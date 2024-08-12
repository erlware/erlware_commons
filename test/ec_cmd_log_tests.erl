%%% @copyright 2024 Erlware, LLC.
-module(ec_cmd_log_tests).

-include("include/ec_cmd_log.hrl").
-include("src/ec_cmd_log.hrl").
-include_lib("eunit/include/eunit.hrl").

should_test() ->
    ErrorLogState = ec_cmd_log:new(error),
    ?assertMatch(true, ec_cmd_log:should(ErrorLogState, ?EC_ERROR)),
    ?assertMatch(true, not ec_cmd_log:should(ErrorLogState, ?EC_INFO)),
    ?assertMatch(true, not ec_cmd_log:should(ErrorLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_ERROR, ec_cmd_log:log_level(ErrorLogState)),
    ?assertEqual(error, ec_cmd_log:atom_log_level(ErrorLogState)),

    InfoLogState = ec_cmd_log:new(info),
    ?assertMatch(true, ec_cmd_log:should(InfoLogState, ?EC_ERROR)),
    ?assertMatch(true, ec_cmd_log:should(InfoLogState, ?EC_INFO)),
    ?assertMatch(true, not ec_cmd_log:should(InfoLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_INFO, ec_cmd_log:log_level(InfoLogState)),
    ?assertEqual(info, ec_cmd_log:atom_log_level(InfoLogState)),

    DebugLogState = ec_cmd_log:new(debug),
    ?assertMatch(true, ec_cmd_log:should(DebugLogState, ?EC_ERROR)),
    ?assertMatch(true, ec_cmd_log:should(DebugLogState, ?EC_INFO)),
    ?assertMatch(true, ec_cmd_log:should(DebugLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_DEBUG, ec_cmd_log:log_level(DebugLogState)),
    ?assertEqual(debug, ec_cmd_log:atom_log_level(DebugLogState)).


no_color_test() ->
    LogState = ec_cmd_log:new(debug, command_line, none),
    ?assertEqual("test",
                 ec_cmd_log:colorize(LogState, ?RED, true, "test")).

color_test() ->
    LogState = ec_cmd_log:new(debug, command_line, high),
    ?assertEqual("\e[1;31m===> test\e[0m",
                 ec_cmd_log:colorize(LogState, ?RED, true, "test")).
