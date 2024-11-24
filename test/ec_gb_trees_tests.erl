%%% @copyright 2024 Erlware, LLC.
-module(ec_gb_trees_tests).
-include_lib("eunit/include/eunit.hrl").

%% For me unit testing initially is about covering the obvious case. A
%% check to make sure that what you expect the tested functionality to
%% do, it actually does. As time goes on and people detect bugs you
%% add tests for those specific problems to the unit test suit.
%%
%% However, when getting started you can only test your basic
%% expectations. So here are the expectations I have for the add
%% functionality.
%%
%% 1) I can put arbitrary terms into the dictionary as keys
%% 2) I can put arbitrary terms into the dictionary as values
%% 3) When I put a value in the dictionary by a key, I can retrieve
%% that same value
%% 4) When I put a different value in the dictionary by key it does
%% not change other key value pairs.
%% 5) When I update a value the new value in available by the new key
%% 6) When a value does not exist a not found exception is created

add_test() ->
    Dict0 = ec_dictionary:new(ec_gb_trees),

    Key1 = foo,
    Key2 = [1, 3],
    Key3 = {"super"},
    Key4 = <<"fabulous">>,
    Key5 = {"Sona", 2, <<"Zuper">>},

    Value1 = Key5,
    Value2 = Key4,
    Value3 = Key2,
    Value4 = Key3,
    Value5 = Key1,

    Dict01 = ec_dictionary:add(Key1, Value1, Dict0),
    Dict02 = ec_dictionary:add(Key3, Value3,
                               ec_dictionary:add(Key2, Value2,
                                                 Dict01)),
    Dict1 =
        ec_dictionary:add(Key5, Value5,
                          ec_dictionary:add(Key4, Value4,
                                            Dict02)),

    ?assertMatch(Value1, ec_dictionary:get(Key1, Dict1)),
    ?assertMatch(Value2, ec_dictionary:get(Key2, Dict1)),
    ?assertMatch(Value3, ec_dictionary:get(Key3, Dict1)),
    ?assertMatch(Value4, ec_dictionary:get(Key4, Dict1)),
    ?assertMatch(Value5, ec_dictionary:get(Key5, Dict1)),


    Dict2 = ec_dictionary:add(Key3, Value5,
                              ec_dictionary:add(Key2, Value4, Dict1)),


    ?assertMatch(Value1, ec_dictionary:get(Key1, Dict2)),
    ?assertMatch(Value4, ec_dictionary:get(Key2, Dict2)),
    ?assertMatch(Value5, ec_dictionary:get(Key3, Dict2)),
    ?assertMatch(Value4, ec_dictionary:get(Key4, Dict2)),
    ?assertMatch(Value5, ec_dictionary:get(Key5, Dict2)),


    ?assertThrow(not_found, ec_dictionary:get(should_blow_up, Dict2)),
    ?assertThrow(not_found, ec_dictionary:get("This should blow up too",
                                              Dict2)).
