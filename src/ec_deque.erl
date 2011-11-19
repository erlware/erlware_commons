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

-module(ec_deque).

-export([new/0, is_empty/1, size/1,
        pushl/2, peekl/1, popl/1,
        pushr/2, peekr/1, popr/1,
        concat/2, foldl/3, foldr/3,
        to_list/1, from_list/1]).

new() ->
    {?MODULE}.

is_empty({?MODULE}) ->
    true;
is_empty({?MODULE, _}) ->
    false;
is_empty({?MODULE, _, _, _}) ->
    false.

size(D) ->
    foldl(fun(_, N) -> N + 1 end, 0, D).

pushl(A, {?MODULE}) ->
    {?MODULE, A};
pushl(A, {?MODULE, B}) ->
    {?MODULE, {digit, A}, {?MODULE}, {digit, B}};
pushl(A, {?MODULE, {digit, B}, M, Sf}) ->
    {?MODULE, {digit, A, B}, M, Sf};
pushl(A, {?MODULE, {digit, B, C}, M, Sf}) ->
    {?MODULE, {digit, A, B, C}, M, Sf};
pushl(A, {?MODULE, {digit, B, C, D}, M, Sf}) ->
    {?MODULE, {digit, A, B, C, D}, M, Sf};
pushl(A, {?MODULE, {digit, B, C, D, E}, M, Sf}) ->
    {?MODULE, {digit, A, B},
        pushl({node, C, D, E}, M), Sf}.

peekl({?MODULE, A}) ->
    A;
peekl({?MODULE, {digit, A}, _M, _Sf}) ->
    A;
peekl({?MODULE, {digit, A, _B}, _M, _Sf}) ->
    A;
peekl({?MODULE, {digit, A, _B, _C}, _M, _Sf}) ->
    A;
peekl({?MODULE, {digit, A, _B, _C, _D}, _M, _Sf}) ->
    A.

popl({?MODULE, _A}) ->
    {?MODULE};
popl({?MODULE, {digit, _A}, {?MODULE}, {digit, B}}) ->
    {?MODULE, B};
popl({?MODULE, {digit, _A}, {?MODULE}, {digit, B, C}}) ->
    {?MODULE, {digit, B}, {?MODULE}, {digit, C}};
popl({?MODULE, {digit, _A}, {?MODULE}, {digit, B, C, D}}) ->
    {?MODULE, {digit, B}, {?MODULE}, {digit, C, D}};
popl({?MODULE, {digit, _A}, {?MODULE}, {digit, B, C, D, E}}) ->
    {?MODULE, {digit, B}, {?MODULE}, {digit, C, D, E}};
popl({?MODULE, {digit, _A}, M, Sf}) ->
    {?MODULE, node_to_digit(peekl(M)), popl(M), Sf};
popl({?MODULE, {digit, _A, B}, M, Sf}) ->
    {?MODULE, {digit, B}, M, Sf};
popl({?MODULE, {digit, _A, B, C}, M, Sf}) ->
    {?MODULE, {digit, B, C}, M, Sf};
popl({?MODULE, {digit, _A, B, C, D}, M, Sf}) ->
    {?MODULE, {digit, B, C, D}, M, Sf}.

pushr(A, {?MODULE}) ->
    {?MODULE, A};
pushr(A, {?MODULE, B}) ->
    {?MODULE, {digit, B}, {?MODULE}, {digit, A}};
pushr(A, {?MODULE, Pr, M, {digit, B}}) ->
    {?MODULE, Pr, M, {digit, B, A}};
pushr(A, {?MODULE, Pr, M, {digit, C, B}}) ->
    {?MODULE, Pr, M, {digit, C, B, A}};
pushr(A, {?MODULE, Pr, M, {digit, D, C, B}}) ->
    {?MODULE, Pr, M, {digit, D, C, B, A}};
pushr(A, {?MODULE, Pr, M, {digit, E, D, C, B}}) ->
    {?MODULE, Pr, pushr({node, E, D, C}, M),
        {digit, B, A}}.

peekr({?MODULE, A}) ->
    A;
peekr({?MODULE, _Pr, _M, {digit, A}}) ->
    A;
peekr({?MODULE, _Pr, _M, {digit, _B, A}}) ->
    A;
peekr({?MODULE, _Pr, _M, {digit, _C, _B, A}}) ->
    A;
peekr({?MODULE, _Pr, _M, {digit, _D, _C, _B, A}}) ->
    A.

popr({?MODULE, _A}) ->
    {?MODULE};
popr({?MODULE, {digit, B}, {?MODULE}, {digit, _A}}) ->
    {?MODULE, B};
popr({?MODULE, {digit, C, B}, {?MODULE}, {digit, _A}}) ->
    {?MODULE, {digit, C}, {?MODULE}, {digit, B}};
popr({?MODULE, {digit, D, C, B}, {?MODULE}, {digit, _A}}) ->
    {?MODULE, {digit, D, C}, {?MODULE}, {digit, B}};
popr({?MODULE, {digit, E, D, C, B}, {?MODULE}, {digit, _A}}) ->
    {?MODULE, {digit, E, D, C}, {?MODULE}, {digit, B}};
popr({?MODULE, Pr, M, {digit, _A}}) ->
    {?MODULE, Pr, popr(M), node_to_digit(peekr(M))};
popr({?MODULE, Pr, M, {digit, B, _A}}) ->
    {?MODULE, Pr, M, {digit, B}};
popr({?MODULE, Pr, M, {digit, C, B, _A}}) ->
    {?MODULE, Pr, M, {digit, C, B}};
popr({?MODULE, Pr, M, {digit, D, C, B, _A}}) ->
    {?MODULE, Pr, M, {digit, D, C, B}}.

node_to_digit({node, A, B}) ->
    {digit, A, B};
node_to_digit({node, A, B, C}) ->
    {digit, A, B, C}.

concat(Xs, Ys) ->
    app3(Xs, {digit}, Ys).

app3({?MODULE}, Ts, Xs) ->
    digit_foldr(fun pushl/2, Xs, Ts, 0);
app3(Xs, Ts, {?MODULE}) ->
    digit_foldl(fun pushr/2, Xs, Ts, 0);
app3({?MODULE, X}, Ts, Xs) ->
    pushl(X, digit_foldr(fun pushl/2, Xs, Ts, 0));
app3(Xs, Ts, {?MODULE, X}) ->
    pushr(X, digit_foldl(fun pushr/2, Xs, Ts, 0));
app3({?MODULE, Pr1, M1, Sf1}, Ts, {?MODULE, Pr2, M2, Sf2}) ->
    {?MODULE, Pr1, app3(M1, digit_app3(Sf1, Ts, Pr2), M2), Sf2}.

digit_foldl(_Fun, Acc, {digit}, _) ->
    Acc;
digit_foldl(Fun, Acc, {digit, A}, 0) ->
    Fun(A, Acc);
digit_foldl(Fun, Acc, {digit, B, A}, 0) ->
    Fun(A, Fun(B, Acc));
digit_foldl(Fun, Acc, {digit, C, B, A}, 0) ->
    Fun(A, Fun(B, Fun(C, Acc)));
digit_foldl(Fun, Acc, {digit, D, C, B, A}, 0) ->
    Fun(A, Fun(B, Fun(C, Fun(D, Acc))));
digit_foldl(Fun, Acc, {digit, A}, N) ->
    node_foldl(Fun, Acc, A, N - 1);
digit_foldl(Fun, Acc0, {digit, A, B}, N) ->
    Acc1 = node_foldl(Fun, Acc0, A, N - 1),
    node_foldl(Fun, Acc1, B, N - 1);
digit_foldl(Fun, Acc0, {digit, A, B, C}, N) ->
    Acc1 = node_foldl(Fun, Acc0, A, N - 1),
    Acc2 = node_foldl(Fun, Acc1, B, N - 1),
    node_foldl(Fun, Acc2, C, N - 1);
digit_foldl(Fun, Acc0, {digit, A, B, C, D}, N) ->
    Acc1 = node_foldl(Fun, Acc0, A, N - 1),
    Acc2 = node_foldl(Fun, Acc1, B, N - 1),
    Acc3 = node_foldl(Fun, Acc2, C, N - 1),
    node_foldl(Fun, Acc3, D, N - 1).

node_foldl(Fun, Acc, {node, B, A}, 0) ->
    Fun(A, Fun(B, Acc));
node_foldl(Fun, Acc, {node, C, B, A}, 0) ->
    Fun(A, Fun(B, Fun(C, Acc)));
node_foldl(Fun, Acc0, {node, A, B}, N) ->
    Acc1 = node_foldl(Fun, Acc0, A, N - 1),
    node_foldl(Fun, Acc1, B, N - 1);
node_foldl(Fun, Acc0, {node, A, B, C}, N) ->
    Acc1 = node_foldl(Fun, Acc0, A, N - 1),
    Acc2 = node_foldl(Fun, Acc1, B, N - 1),
    node_foldl(Fun, Acc2, C, N - 1).

digit_foldr(_Fun, Acc, {digit}, _) ->
    Acc;
digit_foldr(Fun, Acc, {digit, A}, 0) ->
    Fun(A, Acc);
digit_foldr(Fun, Acc, {digit, A, B}, 0) ->
    Fun(A, Fun(B, Acc));
digit_foldr(Fun, Acc, {digit, A, B, C}, 0) ->
    Fun(A, Fun(B, Fun(C, Acc)));
digit_foldr(Fun, Acc, {digit, A, B, C, D}, 0) ->
    Fun(A, Fun(B, Fun(C, Fun(D, Acc))));
digit_foldr(Fun, Acc, {digit, A}, N) ->
    node_foldr(Fun, Acc, A, N - 1);
digit_foldr(Fun, Acc0, {digit, B, A}, N) ->
    Acc1 = node_foldr(Fun, Acc0, A, N - 1),
    node_foldr(Fun, Acc1, B, N - 1);
digit_foldr(Fun, Acc0, {digit, C, B, A}, N) ->
    Acc1 = node_foldr(Fun, Acc0, A, N - 1),
    Acc2 = node_foldr(Fun, Acc1, B, N - 1),
    node_foldr(Fun, Acc2, C, N - 1);
digit_foldr(Fun, Acc0, {digit, D, C, B, A}, N) ->
    Acc1 = node_foldr(Fun, Acc0, A, N - 1),
    Acc2 = node_foldr(Fun, Acc1, B, N - 1),
    Acc3 = node_foldr(Fun, Acc2, C, N - 1),
    node_foldr(Fun, Acc3, D, N - 1).

node_foldr(Fun, Acc, {node, A, B}, 0) ->
    Fun(A, Fun(B, Acc));
node_foldr(Fun, Acc, {node, A, B, C}, 0) ->
    Fun(A, Fun(B, Fun(C, Acc)));
node_foldr(Fun, Acc0, {node, B, A}, N) ->
    Acc1 = node_foldr(Fun, Acc0, A, N - 1),
    node_foldr(Fun, Acc1, B, N - 1);
node_foldr(Fun, Acc0, {node, C, B, A}, N) ->
    Acc1 = node_foldr(Fun, Acc0, A, N - 1),
    Acc2 = node_foldr(Fun, Acc1, B, N - 1),
    node_foldr(Fun, Acc2, C, N - 1).

digit_app3({digit, A},          {digit},             {digit, B}) ->
    {digit, {node, A, B}};
digit_app3({digit, A},          {digit},             {digit, B, C}) ->
    {digit, {node, A, B, C}};
digit_app3({digit, A},          {digit},             {digit, B, C, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A},          {digit},             {digit, B, C, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A},          {digit, B},          {digit, C}) ->
    {digit, {node, A, B, C}};
digit_app3({digit, A},          {digit, B},          {digit, C, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A},          {digit, B},          {digit, C, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A},          {digit, B},          {digit, C, D, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A},          {digit, B, C},       {digit, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A},          {digit, B, C},       {digit, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A},          {digit, B, C},       {digit, D, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A},          {digit, B, C},       {digit, D, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A},          {digit, B, C, D},    {digit, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A},          {digit, B, C, D},    {digit, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A},          {digit, B, C, D},    {digit, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A},          {digit, B, C, D},    {digit, E, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A},          {digit, B, C, D, E}, {digit, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A},          {digit, B, C, D, E}, {digit, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A},          {digit, B, C, D, E}, {digit, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A},          {digit, B, C, D, E}, {digit, F, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B},       {digit},             {digit, C}) ->
    {digit, {node, A, B, C}};
digit_app3({digit, A, B},       {digit},             {digit, C, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A, B},       {digit},             {digit, C, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B},       {digit},             {digit, C, D, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B},       {digit, C},          {digit, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A, B},       {digit, C},          {digit, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B},       {digit, C},          {digit, D, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B},       {digit, C},          {digit, D, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B},       {digit, C, D},       {digit, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B},       {digit, C, D},       {digit, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B},       {digit, C, D},       {digit, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B},       {digit, C, D},       {digit, E, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B},       {digit, C, D, E},    {digit, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B},       {digit, C, D, E},    {digit, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B},       {digit, C, D, E},    {digit, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B},       {digit, C, D, E},    {digit, F, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B},       {digit, C, D, E, F}, {digit, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B},       {digit, C, D, E, F}, {digit, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B},       {digit, C, D, E, F}, {digit, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B},       {digit, C, D, E, F}, {digit, G, H, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C},    {digit},             {digit, D}) ->
    {digit, {node, A, B}, {node, C, D}};
digit_app3({digit, A, B, C},    {digit},             {digit, D, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B, C},    {digit},             {digit, D, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B, C},    {digit},             {digit, D, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C},    {digit, D},          {digit, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B, C},    {digit, D},          {digit, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B, C},    {digit, D},          {digit, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C},    {digit, D},          {digit, E, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C},    {digit, D, E},       {digit, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B, C},    {digit, D, E},       {digit, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C},    {digit, D, E},       {digit, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C},    {digit, D, E},       {digit, F, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C},    {digit, D, E, F},    {digit, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C},    {digit, D, E, F},    {digit, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C},    {digit, D, E, F},    {digit, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C},    {digit, D, E, F},    {digit, G, H, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C},    {digit, D, E, F, G}, {digit, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C},    {digit, D, E, F, G}, {digit, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C},    {digit, D, E, F, G}, {digit, H, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C},    {digit, D, E, F, G}, {digit, H, I, J, K}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}, {node, J, K}};
digit_app3({digit, A, B, C, D}, {digit},             {digit, E}) ->
    {digit, {node, A, B, C}, {node, D, E}};
digit_app3({digit, A, B, C, D}, {digit},             {digit, E, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B, C, D}, {digit},             {digit, E, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C, D}, {digit},             {digit, E, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C, D}, {digit, E},          {digit, F}) ->
    {digit, {node, A, B, C}, {node, D, E, F}};
digit_app3({digit, A, B, C, D}, {digit, E},          {digit, F, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C, D}, {digit, E},          {digit, F, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C, D}, {digit, E},          {digit, F, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C, D}, {digit, E, F},       {digit, G}) ->
    {digit, {node, A, B, C}, {node, D, E}, {node, F, G}};
digit_app3({digit, A, B, C, D}, {digit, E, F},       {digit, G, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C, D}, {digit, E, F},       {digit, G, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C, D}, {digit, E, F},       {digit, G, H, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G},    {digit, H}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G},    {digit, H, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G},    {digit, H, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G},    {digit, H, I, J, K}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}, {node, J, K}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G, H}, {digit, I}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G, H}, {digit, I, J}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H}, {node, I, J}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G, H}, {digit, I, J, K}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}, {node, J, K}};
digit_app3({digit, A, B, C, D}, {digit, E, F, G, H}, {digit, I, J, K, L}) ->
    {digit, {node, A, B, C}, {node, D, E, F}, {node, G, H, I}, {node, J, K, L}}.

foldl(Fun, Acc, D) ->
    foldl(Fun, Acc, D, 0).

foldl(_Fun, Acc, {?MODULE}, _N) ->
    Acc;
foldl(Fun, Acc, {?MODULE, A}, 0) ->
    Fun(A, Acc);
foldl(Fun, Acc, {?MODULE, A}, N) ->
    node_foldl(Fun, Acc, A, N - 1);
foldl(Fun, Acc0, {?MODULE, Pr, M, Sf}, N) ->
    Acc1 = digit_foldl(Fun, Acc0, Pr, N),
    Acc2 = foldl(Fun, Acc1, M, N + 1),
    digit_foldl(Fun, Acc2, Sf, N).

foldr(Fun, Acc, D) ->
    foldr(Fun, Acc, D, 0).

foldr(_Fun, Acc, {?MODULE}, _N) ->
    Acc;
foldr(Fun, Acc, {?MODULE, A}, 0) ->
    Fun(A, Acc);
foldr(Fun, Acc, {?MODULE, A}, N) ->
    node_foldr(Fun, Acc, A, N - 1);
foldr(Fun, Acc0, {?MODULE, Pr, M, Sf}, N) ->
    Acc1 = digit_foldr(Fun, Acc0, Sf, N),
    Acc2 = foldr(Fun, Acc1, M, N + 1),
    digit_foldr(Fun, Acc2, Pr, N).

to_list(D) ->
    foldr(fun(X, L) -> [X | L] end, [], D).

from_list(L) ->
    lists:foldl(fun pushr/2, new(), L).
