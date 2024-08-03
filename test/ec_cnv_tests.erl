%%% @copyright 2024 Erlware, LLC.
-module(ec_cnv_tests).

-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertError(badarg, ec_cnv:to_integer(1.5, strict)).

to_float_test() ->
    ?assertError(badarg, ec_cnv:to_float(10, strict)).

to_atom_test() ->
    ?assertMatch(true, ec_cnv:to_atom("true")),
    ?assertMatch(true, ec_cnv:to_atom(<<"true">>)),
    ?assertMatch(false, ec_cnv:to_atom(<<"false">>)),
    ?assertMatch(false, ec_cnv:to_atom(false)),
    ?assertError(badarg, ec_cnv:to_atom("hello_foo_bar_baz")),

    S = erlang:list_to_atom("1"),
    ?assertMatch(S, ec_cnv:to_atom(1)).

to_boolean_test()->
    ?assertMatch(true, ec_cnv:to_boolean(<<"true">>)),
    ?assertMatch(true, ec_cnv:to_boolean("true")),
    ?assertMatch(true, ec_cnv:to_boolean(true)),
    ?assertMatch(false, ec_cnv:to_boolean(<<"false">>)),
    ?assertMatch(false, ec_cnv:to_boolean("false")),
    ?assertMatch(false, ec_cnv:to_boolean(false)).
