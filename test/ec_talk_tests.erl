%%% @copyright 2024 Erlware, LLC.
-module(ec_talk_tests).

-include_lib("eunit/include/eunit.hrl").

general_test_() ->
    [?_test(42 == ec_talk:get_integer("42")),
     ?_test(500_211 == ec_talk:get_integer("500211")),
     ?_test(1_234_567_890 == ec_talk:get_integer("1234567890")),
     ?_test(12_345_678_901_234_567_890 == ec_talk:get_integer("12345678901234567890")),
     ?_test(true == ec_talk:get_boolean("true")),
     ?_test(false == ec_talk:get_boolean("false")),
     ?_test(true == ec_talk:get_boolean("Ok")),
     ?_test(true == ec_talk:get_boolean("ok")),
     ?_test(true == ec_talk:get_boolean("Y")),
     ?_test(true == ec_talk:get_boolean("y")),
     ?_test(false == ec_talk:get_boolean("False")),
     ?_test(false == ec_talk:get_boolean("No")),
     ?_test(false == ec_talk:get_boolean("no"))].
