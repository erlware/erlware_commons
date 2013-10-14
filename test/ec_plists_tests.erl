%%% @copyright Erlware, LLC.
-module(ec_plists_tests).

-ifdef(DEV_ONLY).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

map_good_test() ->
    Results = ec_plists:map(fun(_) ->
                                    ok
                            end,
                            lists:seq(1, 5)),
    ?assertMatch([ok, ok, ok, ok, ok],
                 Results).

ftmap_good_test() ->
    Results = ec_plists:ftmap(fun(_) ->
                                      ok
                              end,
                              lists:seq(1, 3)),
    ?assertMatch([{value, ok}, {value, ok}, {value, ok}],
                 Results).

filter_good_test() ->
    Results = ec_plists:filter(fun(X) ->
                                       X == show
                               end,
                               [show, show, remove]),
    ?assertMatch([show, show],
                 Results).

map_timeout_test() ->
    ?assertExit(timeout,
                ec_plists:map(fun(T) ->
                                      timer:sleep(T),
                                      T
                              end,
                              [1, 100], {timeout, 10})).

ftmap_timeout_test() ->
    ?assertExit(timeout,
                ec_plists:ftmap(fun(X) ->
                                        timer:sleep(X),
                                        true
                                end,
                                [100, 1], {timeout, 10})).

filter_timeout_test() ->
    ?assertExit(timeout,
                 ec_plists:filter(fun(T) ->
                                          timer:sleep(T),
                                          T == 1
                                  end,
                                  [1, 100], {timeout, 10})).

map_bad_test() ->
    ?assertExit({{nocatch,test_exception}, _},
                 ec_plists:map(fun(_) ->
                                    erlang:throw(test_exception)
                            end,
                            lists:seq(1, 5))).


ftmap_bad_test() ->
    Results =
        ec_plists:ftmap(fun(2) ->
                                erlang:throw(test_exception);
                           (N) ->
                                N
                        end,
                        lists:seq(1, 5)),
    ?assertMatch([{value, 1}, {error,{throw,test_exception}}, {value, 3},
                  {value, 4}, {value, 5}] , Results).

-endif.
