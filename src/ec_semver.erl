%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Helper functions for working with semver versioning strings.
%%%  See http://semver.org/ for the spec.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_semver).

-exports([
	  compare/2
	 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Is semver version string A bigger than version string B?
%% <pre>
%% Example: compare("3.2.5alpha", "3.10.6") returns: false
%% </pre>
-spec compare(VsnA::string(), VsnB::string()) -> boolean().
compare(VsnA, VsnB) ->
    compare_toks(tokens(VsnA),tokens(VsnB)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
tokens(Vsn) ->
    [MajorVsn, MinorVsn, RawPatch] = string:tokens(Vsn, "."),
    {PatchVsn, PatchString} = split_patch(RawPatch),
    {MajorVsn, MinorVsn, PatchVsn, PatchString}.

split_patch(RawPatch) ->
    {PatchVsn, PatchStr} = split_patch(RawPatch, {"", ""}),
    {lists:reverse(PatchVsn), PatchStr}.

split_patch([], Acc) ->
    Acc;
split_patch([Dig|T], {PatchVsn, PatchStr}) when Dig >= $0 andalso Dig =< $9 ->
    split_patch(T, {[Dig|PatchVsn], PatchStr});
split_patch(PatchStr, {PatchVsn, ""}) ->
    {PatchVsn, PatchStr}.

compare_toks({MajA, MinA, PVA, PSA}, {MajB, MinB, PVB, PSB}) ->
    compare_toks2({to_int(MajA), to_int(MinA), to_int(PVA), PSA},
		 {to_int(MajB), to_int(MinB), to_int(PVB), PSB}).

compare_toks2({MajA, _MinA, _PVA, _PSA}, {MajB, _MinB, _PVB, _PSB}) when MajA > MajB ->
    true;
compare_toks2({_Maj, MinA, _PVA, _PSA}, {_Maj, MinB, _PVB, _PSB}) when MinA > MinB ->
    true;
compare_toks2({_Maj, _Min, PVA, _PSA}, {_Maj, _Min, PVB, _PSB}) when PVA > PVB ->
    true;
compare_toks2({_Maj, _Min, _PV, ""}, {_Maj, _Min, _PV, PSB}) when PSB /= ""->
    true;
compare_toks2({_Maj, _Min, _PV, PSA}, {_Maj, _Min, _PV, ""}) when PSA /= ""->
    false;
compare_toks2({_Maj, _Min, _PV, PSA}, {_Maj, _Min, _PV, PSB}) when PSA > PSB ->
    true;
compare_toks2(_ToksA, _ToksB) ->
    false.
    
to_int(String) ->
    case catch list_to_integer(String) of
        Integer when is_integer(Integer) ->
	    Integer;
        _ ->
	    throw(invalid_semver_string)
    end.


%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

split_patch_test() ->
    ?assertMatch({"123", "alpha1"}, split_patch("123alpha1")).

compare_test() ->
    ?assertMatch(true, compare("1.2.3", "1.2.3alpha")),
    ?assertMatch(true, compare("1.2.3beta", "1.2.3alpha")),
    ?assertMatch(true, compare("1.2.4", "1.2.3")),
    ?assertMatch(true, compare("1.3.3", "1.2.3")),
    ?assertMatch(true, compare("2.2.3", "1.2.3")),
    ?assertMatch(true, compare("4.2.3", "3.10.3")),
    ?assertMatch(false, compare("1.2.3", "2.2.3")),
    ?assertThrow(invalid_semver_string, compare("1.b.2", "1.3.4")),
    ?assertThrow(invalid_semver_string, compare("1.2.2", "1.3.t")).

-endif.
