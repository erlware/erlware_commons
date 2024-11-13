%%% @copyright 2024 Erlware, LLC.
-module(ec_file_tests).

-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    Dir = ec_file:insecure_mkdtemp(),
    ec_file:mkdir_path(Dir),
    ?assertMatch(false, ec_file:is_symlink(Dir)),
    ?assertMatch(true, filelib:is_dir(Dir)).

md5sum_test() ->
    ?assertMatch("cfcd208495d565ef66e7dff9f98764da", ec_file:md5sum("0")).

sha1sum_test() ->
    ?assertMatch("b6589fc6ab0dc82cf12099d1c2d40ab994e8410c", ec_file:sha1sum("0")).

file_test() ->
    Dir = ec_file:insecure_mkdtemp(),
    TermFile = filename:join(Dir, "ec_file/dir/file.term"),
    TermFileCopy = filename:join(Dir, "ec_file/dircopy/file.term"),
    filelib:ensure_dir(TermFile),
    filelib:ensure_dir(TermFileCopy),
    ec_file:write_term(TermFile, "term"),
    ?assertMatch({ok, <<"\"term\". ">>}, ec_file:read(TermFile)),
    ec_file:copy(filename:dirname(TermFile),
         filename:dirname(TermFileCopy),
         [recursive]).

teardown_test() ->
    Dir = ec_file:insecure_mkdtemp(),
    ec_file:remove(Dir, [recursive]),
    ?assertMatch(false, filelib:is_dir(Dir)).

setup_base_and_target() ->
    BaseDir = ec_file:insecure_mkdtemp(),
    DummyContents = <<"This should be deleted">>,
    SourceDir = filename:join([BaseDir, "source"]),
    ok = file:make_dir(SourceDir),
    Name1 = filename:join([SourceDir, "fileone"]),
    Name2 = filename:join([SourceDir, "filetwo"]),
    Name3 = filename:join([SourceDir, "filethree"]),
    NoName = filename:join([SourceDir, "noname"]),

    ok = file:write_file(Name1, DummyContents),
    ok = file:write_file(Name2, DummyContents),
    ok = file:write_file(Name3, DummyContents),
    ok = file:write_file(NoName, DummyContents),
    {BaseDir, SourceDir, {Name1, Name2, Name3, NoName}}.

exists_test() ->
    BaseDir = ec_file:insecure_mkdtemp(),
    SourceDir = filename:join([BaseDir, "source1"]),
    NoName = filename:join([SourceDir, "noname"]),
    ok = file:make_dir(SourceDir),
    Name1 = filename:join([SourceDir, "fileone"]),
    ok = file:write_file(Name1, <<"Testn">>),
    ?assertMatch(true, ec_file:exists(Name1)),
    ?assertMatch(false, ec_file:exists(NoName)).

real_path_test() ->
    BaseDir = "foo",
    Dir = filename:absname(filename:join(BaseDir, "source1")),
    LinkDir = filename:join([BaseDir, "link"]),
    ok = ec_file:mkdir_p(Dir),
    file:make_symlink(Dir, LinkDir),
    ?assertEqual(Dir, ec_file:real_dir_path(LinkDir)),
    ?assertEqual(directory, ec_file:type(Dir)),
    ?assertEqual(symlink, ec_file:type(LinkDir)),
    TermFile = filename:join(BaseDir, "test_file"),
    ok = ec_file:write_term(TermFile, foo),
    ?assertEqual(file, ec_file:type(TermFile)),
    ?assertEqual(true, ec_file:is_symlink(LinkDir)),
    ?assertEqual(false, ec_file:is_symlink(Dir)).

find_test() ->
    %% Create a directory in /tmp for the test. Clean everything afterwards
    {BaseDir, _SourceDir, {Name1, Name2, Name3, _NoName}} = setup_base_and_target(),
    Result = ec_file:find(BaseDir, "file[a-z]+\$"),
    ?assertMatch(3, erlang:length(Result)),
    ?assertEqual(true, lists:member(Name1, Result)),
    ?assertEqual(true, lists:member(Name2, Result)),
    ?assertEqual(true, lists:member(Name3, Result)),
    ec_file:remove(BaseDir, [recursive]).
