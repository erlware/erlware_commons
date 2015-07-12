-module(ec_git_vsn_tests).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).


%% Hi! This test only works because you've cloned this repo out of git
%% AND we're tagging this repo with a "v" prefix. Hey, it's better
%% than nothing.
vsn_grep_test() ->
    {ok, VSN} = ec_git_vsn:vsn({"v"}),

    [$v|OtherVSN] = os:cmd("git tag | xargs -I@ git log --format=format:\"%ai @%n\" -1 @ | sort -r | awk '{print $4}' | head -n 1"),

    ?assertEqual(OtherVSN, VSN ++ "\n"),
    ok.
