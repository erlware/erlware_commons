%%%-------------------------------------------------------------------
%%% @doc
%%% simple parrallel map. Originally provided by Joe Armstrong
%%% on the erlang questions mailing list.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_plists).

-export([map/2,
         map/3,
         ftmap/2,
         ftmap/3,
         filter/2,
         filter/3]).

%%=============================================================================
%% Public API
%%=============================================================================

%% @doc Takes a function and produces a list of the result of the function
%%      applied to each element of the argument list. A timeout is optional.
%%      In the event of a timeout or an exception the entire map will fail
%%      with an excption with class throw.
-spec map(fun(), [any()]) -> [any()].
map(Fun, List) ->
    map(Fun, List, infinity).

-spec map(fun(), [any()], non_neg_integer()) -> [any()].
map(Fun, List, Timeout) ->
    run_list_fun_in_parallel(map, Fun, List, Timeout).

%% @doc Takes a function and produces a list of the result of the function
%%      applied to each element of the argument list. A timeout is optional.
%%      This function differes from regular map in that it is fault tolerant.
%%      If a timeout or an exception occurs while processing an element in
%%      the input list the ftmap operation will continue to function. Timeouts
%%      and exceptions will be reflected in the output of this function.
%%      All application level results are wrapped in a tuple with the tag
%%      'value'. Exceptions will come through as they are and timeouts will
%%      return as the atom timeout.
%%      This is useful when the ftmap is being used for side effects.
%% <pre>
%% 2> ftmap(fun(N) -> factorial(N) end, [1, 2, 1000000, "not num"], 100)
%% [{value, 1}, {value, 2}, timeout, {badmatch, ...}]
%% </pre>
-spec ftmap(fun(), [any()]) -> [{value, any()} | any()].
ftmap(Fun, List) ->
    ftmap(Fun, List, infinity).

-spec ftmap(fun(), [any()], non_neg_integer()) -> [{value, any()} | any()].
ftmap(Fun, List, Timeout) ->
    run_list_fun_in_parallel(ftmap, Fun, List, Timeout).

%% @doc Returns a list of the elements in the supplied list which
%%      the function Fun returns true. A timeout is optional. In the
%%      event of a timeout the filter operation fails.
-spec filter(fun(), [any()]) -> [any()].
filter(Fun, List) ->
    filter(Fun, List, infinity).

-spec filter(fun(), [any()], integer()) -> [any()].
filter(Fun, List, Timeout) ->
    run_list_fun_in_parallel(filter, Fun, List, Timeout).

%%=============================================================================
%% Internal API
%%=============================================================================
-spec run_list_fun_in_parallel(atom(), fun(), [any()], integer()) -> [any()].
run_list_fun_in_parallel(ListFun, Fun, List, Timeout) ->
    LocalPid = self(),
    Pids =
        lists:map(fun(E) ->
                          Pid =
                              proc_lib:spawn(fun() ->
                                                     wait(LocalPid, Fun,
                                                          E, Timeout)
                                             end),
                          {Pid, E}
                  end, List),
    gather(ListFun, Pids).

-spec wait(pid(), fun(), any(), integer()) -> any().
wait(Parent, Fun, E, Timeout) ->
    WaitPid = self(),
    Child = spawn(fun() ->
                          do_f(WaitPid, Fun, E)
                  end),

    wait(Parent, Child, Timeout).

-spec wait(pid(), pid(), integer()) -> any().
wait(Parent, Child, Timeout) ->
    receive
        {Child, Ret} ->
            Parent ! {self(), Ret}
    after Timeout ->
            exit(Child, timeout),
            Parent ! {self(), timeout}
    end.

-spec gather(atom(), [any()]) -> [any()].
gather(map, PidElementList) ->
    map_gather(PidElementList);
gather(ftmap, PidElementList) ->
    ftmap_gather(PidElementList);
gather(filter, PidElementList) ->
    filter_gather(PidElementList).

-spec map_gather([pid()]) -> [any()].
map_gather([{Pid, _E} | Rest]) ->
    receive
        {Pid, {value, Ret}} ->
            [Ret|map_gather(Rest)];
                                                % timeouts fall here too. Should timeouts be a return value
                                                % or an exception? I lean toward return value, but the code
                                                % is easier with the exception. Thoughts?
        {Pid, Exception} ->
            killall(Rest),
            throw(Exception)
    end;
map_gather([]) ->
    [].

-spec ftmap_gather([pid()]) -> [any()].
ftmap_gather([{Pid, _E} | Rest]) ->
    receive
        {Pid, Value} -> [Value|ftmap_gather(Rest)]
    end;
ftmap_gather([]) ->
    [].

-spec filter_gather([pid()]) -> [any()].
filter_gather([{Pid, E} | Rest]) ->
    receive
        {Pid, {value, false}} ->
            filter_gather(Rest);
        {Pid, {value, true}} ->
            [E|filter_gather(Rest)];
        {Pid, {value, NotBool}} ->
            killall(Rest),
            throw({bad_return_value, NotBool});
        {Pid, Exception} ->
            killall(Rest),
            throw(Exception)
    end;
filter_gather([]) ->
    [].

-spec do_f(pid(), fun(), any())  -> no_return().
do_f(Parent, F, E) ->
    try
        Result = F(E),
        Parent ! {self(), {value, Result}}
    catch
        _Class:Exception ->
                                                % Losing class info here, but since throw does not accept
                                                % that arg anyhow and forces a class of throw it does not
                                                % matter.
            Parent ! {self(), Exception}
    end.

-spec killall([pid()]) -> ok.
killall([{Pid, _E}|T]) ->
    exit(Pid, kill),
    killall(T);
killall([]) ->
    ok.

%%=============================================================================
%% Tests
%%=============================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

map_good_test() ->
    Results = map(fun(_) ->
                          ok
                  end,
                  lists:seq(1, 5), infinity),
    ?assertMatch([ok, ok, ok, ok, ok],
                 Results).

ftmap_good_test() ->
    Results = ftmap(fun(_) ->
                            ok
                    end,
                    lists:seq(1, 3), infinity),
    ?assertMatch([{value, ok}, {value, ok}, {value, ok}],
                 Results).

filter_good_test() ->
    Results = filter(fun(X) ->
                             X == show
                     end,
                     [show, show, remove], infinity),
    ?assertMatch([show, show],
                 Results).

map_timeout_test() ->
    Results =
        try
            map(fun(T) ->
                        timer:sleep(T),
                        T
                end,
                [1, 100], 10)
        catch
            C:E -> {C, E}
        end,
    ?assertMatch({throw, timeout}, Results).

ftmap_timeout_test() ->
    Results = ftmap(fun(X) ->
                            timer:sleep(X),
                            true
                    end,
                    [100, 1], 10),
    ?assertMatch([timeout, {value, true}], Results).

filter_timeout_test() ->
    Results =
        try
            filter(fun(T) ->
                           timer:sleep(T),
                           T == 1
                   end,
                   [1, 100], 10)
        catch
            C:E -> {C, E}
        end,
    ?assertMatch({throw, timeout}, Results).

map_bad_test() ->
    Results =
        try
            map(fun(_) ->
                        throw(test_exception)
                end,
                lists:seq(1, 5), infinity)
        catch
            C:E -> {C, E}
        end,
    ?assertMatch({throw, test_exception}, Results).

ftmap_bad_test() ->
    Results =
        ftmap(fun(2) ->
                      throw(test_exception);
                 (N) ->
                      N
              end,
              lists:seq(1, 5), infinity),
    ?assertMatch([{value, 1}, test_exception, {value, 3},
                  {value, 4}, {value, 5}] , Results).

-endif.
