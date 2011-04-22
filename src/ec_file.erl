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
	 mkdtemp/0,
	 mkdir_path/1,
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
	      path/0,
	      option/0
	     ]).

-include_lib("kernel/include/file.hrl").

%% User friendly exception message (remove line and module info once we
%% get them in stack traces)
-define(UEX(Exception, UMSG, UVARS),
	{uex, {?MODULE,
	       ?LINE,
	       Exception,
	       lists:flatten(io_lib:fwrite(UMSG, UVARS))}}).

-define(CHECK_PERMS_MSG,
	"Try checking that you have the correct permissions and try again~n").

%%============================================================================
%% Types
%%============================================================================
-type path() :: string().
-type option() :: [atom()].

%%%===================================================================
%%% API
%%%===================================================================
%% @doc copy an entire directory to another location.
-spec copy(path(), path(), Options::[option()]) -> ok.
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
-spec copy(From::string(), To::string()) -> ok.
copy(From, To) ->
    try
	ec_file_copy(From, To)
    catch
	_C:E -> throw(?UEX({copy_failed, E}, ?CHECK_PERMS_MSG, []))
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
-spec remove(path(), Options::[option()]) -> ok | {error, Reason::term()}.
remove(Path, Options) ->
    try
	ok = ec_file_remove(Path, Options)
    catch
	_C:E -> throw(?UEX({remove_failed, E}, ?CHECK_PERMS_MSG, []))
    end.

%% @doc delete a file.
-spec remove(path()) -> ok | {error, Reason::term()}.
remove(Path) ->
    remove(Path, []).

%% @doc indicates witha boolean if the path supplied refers to symlink.
-spec is_symlink(path()) -> boolean().
is_symlink(Path) ->
    case catch file:read_link_info(Path) of
	{ok, #file_info{type = symlink}} ->
	    true;
	_ ->
	    false
    end.

%% @doc make a unique temorory directory. Similar function to BSD stdlib
%% function of the same name.
-spec mkdtemp() -> TmpDirPath::path().
mkdtemp() ->
    UniqueNumber = integer_to_list(element(3, now())),
    TmpDirPath =
	filename:join([tmp(), lists:flatten([".tmp_dir", UniqueNumber])]),
    try
	ok = mkdir_path(TmpDirPath),
	TmpDirPath
    catch
	_C:E -> throw(?UEX({mkdtemp_failed, E}, ?CHECK_PERMS_MSG, []))
    end.


%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_path(path()) -> ok.
mkdir_path(Path) ->
    % We are exploiting a feature of ensuredir that that creates all
    % directories up to the last element in the filename, then ignores
    % that last element. This way we ensure that the dir is created
    % and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    try
	ok = filelib:ensure_dir(DirName)
    catch
	_C:E -> throw(?UEX({mkdir_path_failed, E}, ?CHECK_PERMS_MSG, []))
    end.


%% @doc consult an erlang term file from the file system.
%%      Provide user readible exeption on failure.
-spec consult(FilePath::path()) -> term().
consult(FilePath) ->
    case file:consult(FilePath) of
	{ok, [Term]} ->
	    Term;
	{error, Error} ->
	    Msg = "The file at ~p~n" ++
		  "is either not a valid Erlang term, does not to exist~n" ++
		  "or you lack the permissions to read it. Please check~n" ++
		  "to see if the file exists and that it has the correct~n" ++
		  "permissions~n",
	    throw(?UEX({failed_to_consult_file, {FilePath, Error}},
		       Msg, [FilePath]))
    end.

%% @doc read a file from the file system. Provide UEX exeption on failure.
-spec read(FilePath::string()) -> binary().
read(FilePath) ->
    try
	{ok, FileBin} = file:read_file(FilePath),
	FileBin
    catch
	_C:E -> throw(?UEX({read_failed, {FilePath, E}},
			   "Read failed for the file ~p with ~p~n" ++
			   ?CHECK_PERMS_MSG,
			   [FilePath, E]))
    end.

%% @doc write a file to the file system. Provide UEX exeption on failure.
-spec write(FileName::string(), Contents::string()) -> ok.
write(FileName, Contents) ->
    case file:write_file(FileName, Contents) of
	ok ->
	    ok;
	{error, Reason} ->
	    Msg = "Writing the file ~s to disk failed with reason ~p.~n" ++
		?CHECK_PERMS_MSG,
	    throw(?UEX({write_file_failure, {FileName, Reason}},
		       Msg,
		       [FileName, Reason]))
    end.

%% @doc write a term out to a file so that it can be consulted later.
-spec write_term(string(), term()) -> ok.
write_term(FileName, Term) ->
    write(FileName, lists:flatten(io_lib:fwrite("~p. ", [Term]))).

%% @doc Finds files and directories that match the regexp supplied in
%%  the TargetPattern regexp.
-spec find(FromDir::path(), TargetPattern::string()) -> [path()].
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
-spec find_in_subdirs(path(), string()) -> [path()].
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

-spec ec_file_remove(path(), [{atom(), any()}]) -> ok.
ec_file_remove(Path, Options) ->
    case lists:member(recursive, Options) of
	false -> file:delete(Path);
	true  -> remove_recursive(Path, Options)
    end.

-spec remove_recursive(path(), Options::list()) -> ok.
remove_recursive(Path, Options) ->
    case filelib:is_dir(Path) of
	false ->
	    file:delete(Path);
	true ->
	    lists:foreach(fun(ChildPath) ->
				  remove_recursive(ChildPath, Options)
			  end, filelib:wildcard(filename:join(Path, "*"))),
	    ok = file:del_dir(Path)
    end.

-spec tmp() -> path().
tmp() ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    % XXX TODO better tmp dir for windows perhaps :)
	    "./tmp";
	_SysArch ->
	    "/tmp"
    end.

%% Copy the subfiles of the From directory to the to directory.
-spec copy_subfiles(path(), path(), [option()]) -> ok.
copy_subfiles(From, To, Options) ->
    Fun =
	fun(ChildFrom) ->
		ChildTo = filename:join([To, filename:basename(ChildFrom)]),
		copy(ChildFrom, ChildTo, Options)
	end,
    lists:foreach(Fun, filelib:wildcard(filename:join(From, "*"))).

-spec ec_file_copy(path(), path()) -> ok.
ec_file_copy(From, To) ->
    {ok, _} = file:copy(From, To),
    {ok, FileInfo} = file:read_file_info(From),
    ok = file:write_file_info(To, FileInfo).

-spec make_dir_if_dir(path()) -> ok.
make_dir_if_dir(File) ->
    case filelib:is_dir(File) of
	true  -> ok;
	false -> ok = mkdir_path(File)
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
    case filelib:is_dir("/tmp/ec_file") of
	true ->
	    remove("/tmp/ec_file", [recursive]);
	false ->
	    ok
    end,
    mkdir_path("/tmp/ec_file/dir"),
    ?assertMatch(false, is_symlink("/tmp/ec_file/dir")),
    ?assertMatch(true, filelib:is_dir("/tmp/ec_file/dir")).


md5sum_test() ->
    ?assertMatch("cfcd208495d565ef66e7dff9f98764da", md5sum("0")).

file_test() ->
    TermFile = "/tmp/ec_file/dir/file.term",
    TermFileCopy = "/tmp/ec_file/dircopy/file.term",
    write_term(TermFile, "term"),
    ?assertMatch("term", consult(TermFile)),
    ?assertMatch(<<"\"term\". ">>, read(TermFile)),
    copy(filename:dirname(TermFile),
	 filename:dirname(TermFileCopy),
	 [recursive]),
    ?assertMatch("term", consult(TermFileCopy)).

teardown_test() ->
    remove("/tmp/ec_file", [recursive]),
    ?assertMatch(false, filelib:is_dir("/tmp/ec_file")).

setup_base_and_target() ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
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
    % Create a directory in /tmp for the test. Clean everything afterwards

    {setup,
     fun setup_base_and_target/0,
     fun ({BaseDir, _, _}) ->
	     ewl_file:delete_dir(BaseDir)
     end,
     fun ({BaseDir, _, {Name1, Name2, Name3, _}}) ->
	      ?assertMatch([Name2,
			    Name3,
			    Name1],
			   ewl_file:find(BaseDir, "file[a-z]+\$"))
      end}.

-endif.
