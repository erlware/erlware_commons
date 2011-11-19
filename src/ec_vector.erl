%%
%% Copyright (c) 2010, Gregory Rogers All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(ec_vector).

-export([new/0, is_empty/1, size/1,
        pushl/2, peekl/1, popl/1,
        pushr/2, peekr/1, popr/1,
        concat/2, subvec/3,
        nth/2, insert/3, replace/3,
        foldl/3, foldr/3,
        to_list/1, from_list/1]).

% size monoid
size_meas(_) ->
    1.

-define(size_id, 0).

size_op(A, B) ->
    A + B.

new() ->
    {?MODULE, finger_tree:new(fun size_meas/1, ?size_id, fun size_op/2)}.

is_empty({?MODULE, FT}) ->
    finger_tree:is_empty(FT).

size({?MODULE, FT}) ->
    finger_tree:measure(FT).

pushl(X, {?MODULE, FT}) ->
    {?MODULE, finger_tree:pushl(X, FT)}.

peekl({?MODULE, FT}) ->
    finger_tree:peekl(FT).

popl({?MODULE, FT}) ->
    {?MODULE, finger_tree:popl(FT)}.

pushr(X, {?MODULE, FT}) ->
    {?MODULE, finger_tree:pushr(X, FT)}.

peekr({?MODULE, FT}) ->
    finger_tree:peekr(FT).

popr({?MODULE, FT}) ->
    {?MODULE, finger_tree:popr(FT)}.

concat({?MODULE, FT1}, {?MODULE, FT2}) ->
    {?MODULE, finger_tree:concat(FT1, FT2)}.

subvec(L, H, {?MODULE, FT} = V) when L >= 1, L =< H ->
    subvec(L, H, finger_tree:measure(FT), V).

subvec(1, Sz, Sz, {?MODULE, FT}) ->
    {?MODULE, FT};
subvec(1, H, _Sz, {?MODULE, FT}) ->
    {?MODULE, finger_tree:takewhile(fun(I) -> I =< H end, FT)};
subvec(L, Sz, Sz, {?MODULE, FT}) ->
    {?MODULE, finger_tree:dropwhile(fun(I) -> I < L end, FT)};
subvec(L, H, _Sz, {?MODULE, FT}) ->
    FT1 = finger_tree:takewhile(fun(I) -> I =< H end, FT),
    {?MODULE, finger_tree:dropwhile(fun(I) -> I < L end, FT1)}.

nth(N, {?MODULE, FT}) ->
    FT1 = finger_tree:dropwhile(fun(I) -> I < N end, FT),
    finger_tree:peekl(FT1).

% insert the element such that it becomes the Nth element
insert(N, X, {?MODULE, FT}) ->
    {L, R1} = finger_tree:split(fun(I) -> I < N end, FT),
    R2 = finger_tree:pushl(X, R1),
    {?MODULE, finger_tree:concat(L, R2)}.

replace(N, X, {?MODULE, FT}) ->
    {L, R1} = finger_tree:split(fun(I) -> I < N end, FT),
    R2 = finger_tree:popl(R1),
    R3 = finger_tree:pushl(X, R2),
    {?MODULE, finger_tree:concat(L, R3)}.

foldl(Fun, Acc, {?MODULE, FT}) ->
    finger_tree:foldl(Fun, Acc, FT).

foldr(Fun, Acc, {?MODULE, FT}) ->
    finger_tree:foldr(Fun, Acc, FT).

to_list({?MODULE, FT}) ->
    finger_tree:foldr(fun(X, L) -> [X | L] end, [], FT).

from_list(L) ->
    {?MODULE, lists:foldl(fun finger_tree:pushr/2,
            finger_tree:new(fun size_meas/1, ?size_id, fun size_op/2), L)}.
