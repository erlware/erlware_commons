%%% @copyright 2024 Erlware, LLC.
-module(ec_lists_tests).

-include_lib("eunit/include/eunit.hrl").

find1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = ec_lists:find(fun(5) ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, 5}, Result),

    Result2 = ec_lists:find(fun(37) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).

find2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = ec_lists:find(fun("five") ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, "five"}, Result),

    Result2 = ec_lists:find(fun(super_duper) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).

find3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
                {"six", 6}],
    Result = ec_lists:find(fun({"one", 1}) ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, {"one", 1}}, Result),

    Result2 = ec_lists:find(fun([fo, bar, baz]) ->
                           true;
                      ({"onehundred", 100}) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).

fetch1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = ec_lists:fetch(fun(5) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(5, Result),

    ?assertThrow(not_found,
                 ec_lists:fetch(fun(37) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

fetch2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = ec_lists:fetch(fun("five") ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch("five", Result),

    ?assertThrow(not_found,
                 ec_lists:fetch(fun(super_duper) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

fetch3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
                {"six", 6}],
    Result = ec_lists:fetch(fun({"one", 1}) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch({"one", 1}, Result),

    ?assertThrow(not_found,
                 ec_lists:fetch(fun([fo, bar, baz]) ->
                               true;
                          ({"onehundred", 100}) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

search1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = ec_lists:search(fun(5) ->
                            {ok, 5};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch({ok, 5, 5}, Result),

    Result2 = ec_lists:search(fun(37) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch(not_found, Result2).

search2_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = ec_lists:search(fun(1) ->
                            {ok, 10};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch({ok, 10, 1}, Result),

    Result2 = ec_lists:search(fun(6) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch({ok, 37, 6}, Result2).

search3_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = ec_lists:search(fun(10) ->
                            {ok, 10};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch(not_found, Result),

    Result2 = ec_lists:search(fun(-1) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch(not_found, Result2).
