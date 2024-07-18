%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2024 Erlware, LLC.
%%% @doc
%%%  Provides a signature to manage returning semver formatted versions
%%%  from various version control repositories.
%%%
%%%  This interface is a member of the Erlware Commons Library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_vsn).

%% API
-export([new/1,
         vsn/1]).

-export_type([t/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(t, {callback, data}).

%% This should be opaque, but that kills dialyzer so for now we export it
%% however you should not rely on the internal representation here
-type t() :: #t{}.

-callback new() -> any().
-callback vsn(any()) -> {ok, string()} | {error, Reason::any()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour.
%%
%% @param ModuleName The module name.
-spec new(module()) -> t().
new(ModuleName) when erlang:is_atom(ModuleName) ->
    #t{callback = ModuleName, data = ModuleName:new()}.

%% @doc Return the semver or an error depending on what is possible
%% with this implementation in this directory.
%%
%% @param The dictionary object
-spec vsn(t()) ->  {ok, string()} | {error, Reason::any()}.
vsn(#t{callback = Mod, data = Data}) ->
    Mod:vsn(Data).
