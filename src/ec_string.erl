%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Helper functions for working with strings.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_string).

-export([
	 compare_versions/2
	]).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Is arbitrary version string A bigger than version string B?
%% Valid version string elements are either separated by . or - or both.
%% Final version string elements may have a numeric followed directly by an
%% alpha numeric and will be compared separately as in 12alpha.
%%
%% <pre>
%% Example: compare_versions("3-2-5-alpha", "3.10.6") will return false
%%          compare_versions("3-2-alpha", "3.2.1-alpha") will return false
%%          compare_versions("3-2alpha", "3.2.1-alpha") will return false
%%          compare_versions("3.2.2", "3.2.2") will return false
%%          compare_versions("3.2.1", "3.2.1-rc2") will return true
%%          compare_versions("3.2.2", "3.2.1") will return true
%% </pre>
-spec compare_versions(VsnA::string(), VsnB::string()) -> boolean().
compare_versions(VsnA, VsnB) ->
    compare(string:tokens(VsnA, ".-"),string:tokens(VsnB, ".-")).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec compare(string(), string()) -> boolean().
compare([Str|TA], [Str|TB]) ->
    compare(TA, TB);
compare([StrA|TA], [StrB|TB]) ->
    fine_compare(split_numeric_alpha(StrA), TA,
		 split_numeric_alpha(StrB), TB);
compare([], [Str]) ->
    not compare_against_nothing(Str);
compare([Str], []) ->
    compare_against_nothing(Str);
compare([], [_,_|_]) ->
    false;
compare([_,_|_], []) ->
    true;
compare([], []) ->
    false.

-spec compare_against_nothing(string()) -> boolean().
compare_against_nothing(Str) ->
    case split_numeric_alpha(Str) of
	{_StrDig, ""} -> true;
	{"", _StrAlpha} -> false;
	{_StrDig, _StrAlpha} -> true
    end.

-spec fine_compare({string(), string()}, string(),
		   {string(), string()}, string()) ->
    boolean().
fine_compare({_StrDigA, StrA}, TA, {_StrDigB, _StrB}, _TB) when StrA /= "", TA /= [] ->
    throw(invalid_version_string);
fine_compare({_StrDigA, _StrA}, _TA, {_StrDigB, StrB}, TB) when StrB /= "", TB /= [] ->
    throw(invalid_version_string);
fine_compare({"", _StrA}, _TA, {StrDigB, _StrB}, _TB) when StrDigB /= "" ->
    false;
fine_compare({StrDigA, _StrA}, _TA, {"", _StrB}, _TB) when StrDigA /= "" ->
    true;
fine_compare({StrDig, ""}, _TA, {StrDig, StrB}, _TB) when StrB /= "" ->
    true;
fine_compare({StrDig, StrA}, _TA, {StrDig, ""}, _TB) when StrA /= "" ->
    false;
fine_compare({StrDig, StrA}, _TA, {StrDig, StrB}, _TB) ->
    StrA > StrB;
fine_compare({StrDigA, _StrA}, _TA, {StrDigB, _StrB}, _TB) ->
    list_to_integer(StrDigA) > list_to_integer(StrDigB).

%% In the case of a version sub part with a numeric then an alpha,
%% split out the numeric and alpha "24alpha" becomes {"24", "alpha"}
-spec split_numeric_alpha(string()) ->
    {PatchVsn::string(), PatchStr::string()}.
split_numeric_alpha(RawVsn) ->
    {Num, Str} = split_numeric_alpha(RawVsn, {"", ""}),
    {lists:reverse(Num), Str}.

-spec split_numeric_alpha(string(), {PatchVsnAcc::string(),
				     PatchStrAcc::string()}) ->
    {PatchVsn::string(), PatchStr::string()}.
split_numeric_alpha([], Acc) ->
    Acc;
split_numeric_alpha([Dig|T], {PatchVsn, PatchStr})
  when Dig >= $0 andalso Dig =< $9 ->
    split_numeric_alpha(T, {[Dig|PatchVsn], PatchStr});
split_numeric_alpha(PatchStr, {PatchVsn, ""}) ->
    {PatchVsn, PatchStr}.


%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

split_numeric_alpha_test() ->
    ?assertMatch({"123", "alpha1"}, split_numeric_alpha("123alpha1")).

compare_versions_test() ->
    ?assertMatch(true, compare_versions("1.2.3", "1.2.3alpha")),
    ?assertMatch(true, compare_versions("1.2.3-beta", "1.2.3-alpha")),
    ?assertMatch(true, compare_versions("1-2-3", "1-2-3alpha")),
    ?assertMatch(true, compare_versions("1-2-3", "1-2-3-rc3")),
    ?assertMatch(true, compare_versions("1.2.3beta", "1.2.3alpha")),
    ?assertMatch(true, compare_versions("1.2.4", "1.2.3")),
    ?assertMatch(true, compare_versions("1.3.3", "1.2.3")),
    ?assertMatch(true, compare_versions("2.2.3", "1.2.3")),
    ?assertMatch(true, compare_versions("4.2.3", "3.10.3")),
    ?assertMatch(false, compare_versions("1.2.3", "2.2.3")),
    ?assertMatch(false, compare_versions("1.2.2", "1.3.t")),
    ?assertMatch(false, compare_versions("1.2t", "1.3.t")),
    ?assertThrow(invalid_version_string, compare_versions("1.b.2", "1.3.4")).

-endif.
