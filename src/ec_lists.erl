%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Provides useful functionionality on standard lists that are
%%%  not provided in the standard library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_lists).

%% API
-export([find/2,
	 fetch/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Find a value in the list with the specified function. If the
%% function returns the atom true, the value is returned as {ok,
%% term()} and processing is aborted, if the function returns false,
%% processing continues until the end of the list. If the end is found
%% and the function never returns true the atom error is returned.
-spec find([term()], fun()) -> {ok, term()} | error.
find([Head | Tail], Fun)
  when is_function(Fun) ->
    case Fun(Head) of
	true ->
	    {ok, Head};
	false ->
	    find(Tail, Fun)
    end;
find([], _Fun) ->
    error.

%% @doc Fetch a value from the list. If the function returns true the
%% value is returend. If processing reaches the end of the list and
%% the function has never returned true an exception not_found is
%% thrown.
-spec fetch(list(), fun()) -> term().
fetch(List, Fun)
  when is_list(List),
       is_function(Fun) ->
    case find(List, Fun) of
	{ok, Head} ->
	    Head;
	error ->
	    throw(not_found)
    end.

%%%===================================================================
%%% API
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

find1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = find(TestData,
		  fun(5) ->
			  true;
		     (_) ->
			  false
		  end),
    ?assertMatch({ok, 5}, Result),

    Result2 = find(TestData,
		   fun(37) ->
			   true;
		      (_) ->
			   false
		   end),
    ?assertMatch(error, Result2).

find2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = find(TestData,
		  fun("five") ->
			  true;
		     (_) ->
			  false
		  end),
    ?assertMatch({ok, "five"}, Result),

    Result2 = find(TestData,
		   fun(super_duper) ->
			   true;
		      (_) ->
			   false
		   end),
    ?assertMatch(error, Result2).



find3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
		{"six", 6}],
    Result = find(TestData,
		  fun({"one", 1}) ->
			  true;
		     (_) ->
			  false
		  end),
    ?assertMatch({ok, {"one", 1}}, Result),

    Result2 = find(TestData,
		   fun([fo, bar, baz]) ->
			   true;
		      ({"onehundred", 100}) ->
			   true;
		      (_) ->
			   false
		   end),
    ?assertMatch(error, Result2).



fetch1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = fetch(TestData,
		  fun(5) ->
			  true;
		     (_) ->
			  false
		  end),
    ?assertMatch(5, Result),

    ?assertThrow(not_found,
		 fetch(TestData,
		       fun(37) ->
			       true;
			  (_) ->
			       false
		       end)).

fetch2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = fetch(TestData,
		   fun("five") ->
			  true;
		      (_) ->
			   false
		   end),
    ?assertMatch("five", Result),

    ?assertThrow(not_found,
		 fetch(TestData,
		       fun(super_duper) ->
			       true;
			  (_) ->
			       false
		       end)).

fetch3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
		{"six", 6}],
    Result = fetch(TestData,
		  fun({"one", 1}) ->
			  true;
		     (_) ->
			  false
		  end),
    ?assertMatch({"one", 1}, Result),

    ?assertThrow(not_found,
		 fetch(TestData,
		       fun([fo, bar, baz]) ->
			       true;
			  ({"onehundred", 100}) ->
			       true;
			  (_) ->
			       false
		       end)).




-endif.
