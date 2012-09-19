%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Helper functions for working with files.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_file).

-export([
         copy/2,
         copy/3,
         insecure_mkdtemp/0,
         mkdir_path/1,
         mkdir_p/1,
         find/2,
         is_symlink/1,
         remove/1,
         remove/2,
         md5sum/1,
         read/1,
         write/2,
         write_term/2,
         consult/1
        ]).

-export_type([
              option/0
             ]).

-include_lib("kernel/include/file.hrl").

-define(CHECK_PERMS_MSG,
        "Try checking that you have the correct permissions and try again~n").

%%============================================================================
%% Types
%%============================================================================
-type option() :: recursive.
-type void() :: ok.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc copy an entire directory to another location.
-spec copy(file:name(), file:name(), Options::[option()]) -> void().
copy(From, To, []) ->
    copy(From, To);
copy(From, To, [recursive] = Options) ->
    case filelib:is_dir(From) of
        false ->
            copy(From, To);
        true ->
            make_dir_if_dir(To),
            copy_subfiles(From, To, Options)
    end.

%% @doc copy a file including timestamps,ownership and mode etc.
-spec copy(From::file:filename(), To::file:filename()) -> ok | {error, Reason::term()}.
copy(From, To) ->
    case file:copy(From, To) of
        {ok, _} ->
            case file:read_file_info(From) of
                {ok, FileInfo} ->
                    case file:write_file_info(To, FileInfo) of
                        ok ->
                            ok;
                        {error, WFError} ->
                            {error, {write_file_info_failed, WFError}}
                    end;
                {error, RFError} ->
                    {error, {read_file_info_failed, RFError}}
            end;
        {error, Error} ->
            {error, {copy_failed, Error}}
    end.

%% @doc return an md5 checksum string or a binary. Same as unix utility of
%%      same name.
-spec md5sum(string() | binary()) -> string().
md5sum(Value) ->
    hex(binary_to_list(erlang:md5(Value))).

%% @doc delete a file. Use the recursive option for directories.
%% <pre>
%% Example: remove("./tmp_dir", [recursive]).
%% </pre>
-spec remove(file:name(), Options::[option()]) -> ok | {error, Reason::term()}.
remove(Path, Options) ->
    case lists:member(recursive, Options) of
        false -> file:delete(Path);
        true  -> remove_recursive(Path, Options)
    end.


%% @doc delete a file.
-spec remove(file:name()) -> ok | {error, Reason::term()}.
remove(Path) ->
    remove(Path, []).

%% @doc indicates witha boolean if the path supplied refers to symlink.
-spec is_symlink(file:name()) -> boolean().
is_symlink(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = symlink}} ->
            true;
        _ ->
            false
    end.


%% @doc make a unique temorory directory. Similar function to BSD stdlib
%% function of the same name.
-spec insecure_mkdtemp() -> TmpDirPath::file:name().
insecure_mkdtemp() ->
    random:seed(now()),
    UniqueNumber = erlang:integer_to_list(erlang:trunc(random:uniform() * 1000000000000)),
    TmpDirPath =
        filename:join([tmp(), lists:flatten([".tmp_dir", UniqueNumber])]),

    case mkdir_path(TmpDirPath) of
        ok -> TmpDirPath;
        Error -> Error
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(file:name()) -> ok | {error, Reason::term()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).


%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_path(file:name()) -> ok | {error, Reason::term()}.
mkdir_path(Path) ->
    mkdir_p(Path).


%% @doc consult an erlang term file from the file system.
%%      Provide user readible exeption on failure.
-spec consult(FilePath::file:name()) -> term().
consult(FilePath) ->
    case file:consult(FilePath) of
        {ok, [Term]} ->
            Term;
        Error ->
            Error
    end.
%% @doc read a file from the file system. Provide UEX exeption on failure.
-spec read(FilePath::file:filename()) -> binary() | {error, Reason::term()}.
read(FilePath) ->
    %% Now that we are moving away from exceptions again this becomes
    %% a bit redundant but we want to be backwards compatible as much
    %% as possible in the api.
    file:read_file(FilePath).


%% @doc write a file to the file system. Provide UEX exeption on failure.
-spec write(FileName::file:filename(), Contents::string()) -> ok | {error, Reason::term()}.
write(FileName, Contents) ->
    %% Now that we are moving away from exceptions again this becomes
    %% a bit redundant but we want to be backwards compatible as much
    %% as possible in the api.
    file:write_file(FileName, Contents).

%% @doc write a term out to a file so that it can be consulted later.
-spec write_term(file:filename(), term()) -> ok | {error, Reason::term()}.
write_term(FileName, Term) ->
    write(FileName, lists:flatten(io_lib:fwrite("~p. ", [Term]))).

%% @doc Finds files and directories that match the regexp supplied in
%%  the TargetPattern regexp.
-spec find(FromDir::file:name(), TargetPattern::string()) -> [file:name()].
find([], _) ->
    [];
find(FromDir, TargetPattern) ->
    case filelib:is_dir(FromDir) of
        false ->
            case re:run(FromDir, TargetPattern) of
                {match, _} -> [FromDir];
                _ -> []
            end;
        true ->
            FoundDir = case re:run(FromDir, TargetPattern) of
                           {match, _} -> [FromDir];
                           _ -> []
                       end,
            List = find_in_subdirs(FromDir, TargetPattern),
            FoundDir ++ List
    end.
%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec find_in_subdirs(file:name(), string()) -> [file:name()].
find_in_subdirs(FromDir, TargetPattern) ->
    lists:foldl(fun(CheckFromDir, Acc)
                      when CheckFromDir == FromDir ->
                        Acc;
                   (ChildFromDir, Acc) ->
                        case find(ChildFromDir, TargetPattern) of
                            []  -> Acc;
                            Res -> Res ++ Acc
                        end
                end,
                [],
                filelib:wildcard(filename:join(FromDir, "*"))).


-spec remove_recursive(file:name(), Options::list()) -> ok | {error, Reason::term()}.
remove_recursive(Path, Options) ->
    case filelib:is_dir(Path) of
        false ->
            file:delete(Path);
        true ->
            lists:foreach(fun(ChildPath) ->
                                  remove_recursive(ChildPath, Options)
                          end, filelib:wildcard(filename:join(Path, "*"))),
            file:del_dir(Path)
    end.

-spec tmp() -> file:name().
tmp() ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            "./tmp";
        _SysArch ->
            "/tmp"
    end.

%% Copy the subfiles of the From directory to the to directory.
-spec copy_subfiles(file:name(), file:name(), [option()]) -> void().
copy_subfiles(From, To, Options) ->
    Fun =
        fun(ChildFrom) ->
                ChildTo = filename:join([To, filename:basename(ChildFrom)]),
                copy(ChildFrom, ChildTo, Options)
        end,
    lists:foreach(Fun, filelib:wildcard(filename:join(From, "*"))).

-spec make_dir_if_dir(file:name()) -> ok | {error, Reason::term()}.
make_dir_if_dir(File) ->
    case filelib:is_dir(File) of
        true  -> ok;
        false -> mkdir_path(File)
    end.

%% @doc convert a list of integers into hex.
-spec hex(string() | non_neg_integer()) -> string().
hex(L) when is_list (L) ->
    lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
    [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I)               ->
    [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I)  -> $0 + I.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    Dir = insecure_mkdtemp(),
    mkdir_path(Dir),
    ?assertMatch(false, is_symlink(Dir)),
    ?assertMatch(true, filelib:is_dir(Dir)).

md5sum_test() ->
    ?assertMatch("cfcd208495d565ef66e7dff9f98764da", md5sum("0")).

file_test() ->
    Dir = insecure_mkdtemp(),
    TermFile = filename:join(Dir, "ec_file/dir/file.term"),
    TermFileCopy = filename:join(Dir, "ec_file/dircopy/file.term"),
    filelib:ensure_dir(TermFile),
    filelib:ensure_dir(TermFileCopy),
    write_term(TermFile, "term"),
    ?assertMatch("term", consult(TermFile)),
    ?assertMatch({ok, <<"\"term\". ">>}, read(TermFile)),
    copy(filename:dirname(TermFile),
         filename:dirname(TermFileCopy),
         [recursive]),
    ?assertMatch("term", consult(TermFileCopy)).

teardown_test() ->
    Dir = insecure_mkdtemp(),
    remove(Dir, [recursive]),
    ?assertMatch(false, filelib:is_dir(Dir)).

setup_base_and_target() ->
    BaseDir = insecure_mkdtemp(),
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

find_test() ->
    %% Create a directory in /tmp for the test. Clean everything afterwards
    {BaseDir, _SourceDir, {Name1, Name2, Name3, _NoName}} = setup_base_and_target(),
    ?assertMatch([Name2,
                  Name3,
                  Name1],
                 find(BaseDir, "file[a-z]+\$")),
    remove(BaseDir, [recursive]).

-endif.
