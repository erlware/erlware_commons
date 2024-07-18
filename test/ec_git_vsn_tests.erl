%%% @copyright 2024 Erlware, LLC.
-module(ec_git_vsn_tests).

-include_lib("eunit/include/eunit.hrl").

parse_tags_test() ->
    ?assertEqual({undefined, ""}, ec_git_vsn:parse_tags("a.b.c")).

get_patch_count_test() ->
    ?assertEqual(0, ec_git_vsn:get_patch_count("a.b.c")).

collect_default_refcount_test() ->
    ?assertMatch({"", _, _}, ec_git_vsn:collect_default_refcount("a.b.c")).
