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

-module(ec_finger_tree).

-export([new/3, is_empty/1, size/1, measure/1,
        pushl/2, peekl/1, popl/1,
        pushr/2, peekr/1, popr/1,
        concat/2, split/2, dropwhile/2, takewhile/2,
        foldl/3, foldr/3,
        to_list/1]).

-type(ft_digit(X) :: {digit, X} | {digit, X, X} | {digit, X, X, X} |
    {digit, X, X, X, X}).
-type(ft_node(V, X) :: {node, V, X, X} | {node, V, X, X, X}).
-type(ft_tree(V, X) :: {tree} | {tree, X} |
    {tree, V, ft_digit(X), ft_tree(V, ft_node(V, X)), ft_digit(X)}).
-type(meas_fun(V, X) :: fun((X) -> V)).
-type(op_fun(V, X) :: fun((X, X) -> V)).
-type(pred_fun(V) :: fun((V) -> boolean())).
-type(finger_tree(V, X) ::
    {?MODULE, {meas_fun(V, X), V, op_fun(V, X)}, ft_tree(V, X)}).

-spec new(meas_fun(V, X), V, op_fun(V, X)) -> finger_tree(V, X).
new(Meas, Id, Op) ->
    {?MODULE, {Meas, Id, Op}, {tree}}.

-spec is_empty(finger_tree(_, _)) -> boolean().
is_empty({?MODULE, _, {tree}}) ->
    true;
is_empty({?MODULE, _, {tree, _}}) ->
    false;
is_empty({?MODULE, _, {tree, _, _, _, _}}) ->
    false.

tree_is_empty({tree}) ->
    true;
tree_is_empty({tree, _}) ->
    false;
tree_is_empty({tree, _, _, _, _}) ->
    false.

-spec size(finger_tree(_, _)) -> non_neg_integer().
size({?MODULE, _Cb, Tree}) ->
    tree_foldl(fun(_, N) -> N + 1 end, 0, Tree, 1).

-spec measure(finger_tree(V, _X)) -> V.
measure({?MODULE, Cb, Tree}) ->
    meas(Cb, Tree, 1).

-spec pushl(X, finger_tree(V, X)) -> finger_tree(V, X).
pushl(A, {?MODULE, Cb, Tree}) ->
    {?MODULE, Cb, tree_pushl(Cb, A, Tree, 1)}.

tree_pushl(_Cb, A, {tree}, _H) ->
    {tree, A};
tree_pushl(Cb, A, {tree, B}, H) ->
    deep(Cb, {digit, A}, {tree}, {digit, B}, H);
tree_pushl(Cb, A, {tree, _, {digit, B}, M, Sf}, H) ->
    deep(Cb, {digit, A, B}, M, Sf, H);
tree_pushl(Cb, A, {tree, _, {digit, B, C}, M, Sf}, H) ->
    deep(Cb, {digit, A, B, C}, M, Sf, H);
tree_pushl(Cb, A, {tree, _, {digit, B, C, D}, M, Sf}, H) ->
    deep(Cb, {digit, A, B, C, D}, M, Sf, H);
tree_pushl(Cb, A, {tree, _, {digit, B, C, D, E}, M, Sf}, H) ->
    deep(Cb, {digit, A, B}, tree_pushl(Cb, node3(Cb, C, D, E, H), M, H + 1), Sf, H).

deep({_Meas, _Id, Op} = Cb, Pr, M, Sf, H) ->
    Vpr = meas(Cb, Pr, H),
    Vsf = meas(Cb, Sf, H),
    V = Op(op_tree(Cb, Vpr, M, H), Vsf),
    {tree, V, Pr, M, Sf}.

op_tree({_, _, _}, V, {tree}, _H) ->
    V;
op_tree({_Meas, _Id, Op} = Cb, V, {tree, A}, H) ->
    Op(V, meas(Cb, A, H));
op_tree({_Meas, _Id, Op}, V1, {tree, V2, _Pr, _M, _Sf}, _H) ->
    Op(V1, V2).

node2({_Meas, _Id, Op} = Cb, A, B, H) ->
    Va = meas(Cb, A, H - 1),
    Vb = meas(Cb, B, H - 1),
    V = Op(Va, Vb),
    {node, V, A, B}.

node3({_Meas, _Id, Op} = Cb, A, B, C, H) ->
    Va = meas(Cb, A, H - 1),
    Vb = meas(Cb, B, H - 1),
    Vc = meas(Cb, C, H - 1),
    V = Op(Va, Op(Vb, Vc)),
    {node, V, A, B, C}.

meas({Meas, _Id, _Op}, X, 0) ->
    Meas(X);
meas({_Meas, _Id, _Op}, {node, V, _A, _B}, _H) ->
    V;
meas({_Meas, _Id, _Op}, {node, V, _A, _B, _C}, _H) ->
    V;
meas({_Meas, _Id, _Op} = Cb, {digit, A}, H) ->
    meas(Cb, A, H - 1);
meas({_Meas, _Id, Op} = Cb, {digit, A, B}, H) ->
    Va = meas(Cb, A, H - 1),
    Vb = meas(Cb, B, H - 1),
    Op(Va, Vb);
meas({_Meas, _Id, Op} = Cb, {digit, A, B, C}, H) ->
    Va = meas(Cb, A, H - 1),
    Vb = meas(Cb, B, H - 1),
    Vc = meas(Cb, C, H - 1),
    Op(Va, Op(Vb, Vc));
meas({_Meas, _Id, Op} = Cb, {digit, A, B, C, D}, H) ->
    Va = meas(Cb, A, H - 1),
    Vb = meas(Cb, B, H - 1),
    Vc = meas(Cb, C, H - 1),
    Vd = meas(Cb, D, H - 1),
    Op(Va, Op(Vb, Op(Vc, Vd)));
meas({_Meas, Id, _Op}, {tree}, _H) ->
    Id;
meas({_Meas, _Id, _Op} = Cb, {tree, A}, H) ->
    meas(Cb, A, H - 1);
meas({_Meas, _Id, _Op}, {tree, V, _Pr, _M, _Sf}, _H) ->
    V.

-spec peekl(finger_tree(V, X)) -> finger_tree(V, X).
peekl({?MODULE, _Cb, Tree}) ->
    tree_peekl(Tree).

tree_peekl({tree, A}) ->
    A;
tree_peekl({tree, _, {digit, A}, _M, _Sf}) ->
    A;
tree_peekl({tree, _, {digit, A, _B}, _M, _Sf}) ->
    A;
tree_peekl({tree, _, {digit, A, _B, _C}, _M, _Sf}) ->
    A;
tree_peekl({tree, _, {digit, A, _B, _C, _D}, _M, _Sf}) ->
    A.

-spec popl(finger_tree(V, X)) -> finger_tree(V, X).
popl({?MODULE, Cb, Tree}) ->
    {?MODULE, Cb, tree_popl(Cb, Tree, 1)}.

tree_popl(_Cb, {tree, _A}, _H) ->
    {tree};
tree_popl(_Cb, {tree, _, {digit, _A}, {tree}, {digit, B}}, _H) ->
    {tree, B};
tree_popl(Cb, {tree, _, {digit, _A}, {tree}, {digit, B, C}}, H) ->
    deep(Cb, {digit, B}, {tree}, {digit, C}, H);
tree_popl(Cb, {tree, _, {digit, _A}, {tree}, {digit, B, C, D}}, H) ->
    deep(Cb, {digit, B, C}, {tree}, {digit, D}, H);
tree_popl(Cb, {tree, _, {digit, _A}, {tree}, {digit, B, C, D, E}}, H) ->
    deep(Cb, {digit, B, C}, {tree}, {digit, D, E}, H);
tree_popl(Cb, {tree, _, {digit, _A}, M, Sf}, H) ->
    deep(Cb, node_to_digit(tree_peekl(M)), tree_popl(Cb, M, H + 1), Sf, H);
tree_popl(Cb, {tree, _, {digit, _A, B}, M, Sf}, H) ->
    deep(Cb, {digit, B}, M, Sf, H);
tree_popl(Cb, {tree, _, {digit, _A, B, C}, M, Sf}, H) ->
    deep(Cb, {digit, B, C}, M, Sf, H);
tree_popl(Cb, {tree, _, {digit, _A, B, C, D}, M, Sf}, H) ->
    deep(Cb, {digit, B, C, D}, M, Sf, H).

node_to_digit({node, _, A, B}) ->
    {digit, A, B};
node_to_digit({node, _, A, B, C}) ->
    {digit, A, B, C}.

-spec pushr(X, finger_tree(V, X)) -> finger_tree(V, X).
pushr(A, {?MODULE, Cb, Tree}) ->
    {?MODULE, Cb, tree_pushr(Cb, A, Tree, 1)}.

tree_pushr(_Cb, A, {tree}, _H) ->
    {tree, A};
tree_pushr(Cb, B, {tree, A}, H) ->
    deep(Cb, {digit, A}, {tree}, {digit, B}, H);
tree_pushr(Cb, B, {tree, _, Pr, M, {digit, A}}, H) ->
    deep(Cb, Pr, M, {digit, A, B}, H);
tree_pushr(Cb, C, {tree, _, Pr, M, {digit, A, B}}, H) ->
    deep(Cb, Pr, M, {digit, A, B, C}, H);
tree_pushr(Cb, D, {tree, _, Pr, M, {digit, A, B, C}}, H) ->
    deep(Cb, Pr, M, {digit, A, B, C, D}, H);
tree_pushr(Cb, E, {tree, _, Pr, M, {digit, A, B, C, D}}, H) ->
    deep(Cb, Pr, tree_pushr(Cb, node3(Cb, A, B, C, H), M, H + 1), {digit, D, E}, H).

-spec peekr(finger_tree(V, X)) -> finger_tree(V, X).
peekr({?MODULE, _Cb, Tree}) ->
    tree_peekr(Tree).

tree_peekr({tree, A}) ->
    A;
tree_peekr({tree, _, _Pr, _M, {digit, A}}) ->
    A;
tree_peekr({tree, _, _Pr, _M, {digit, _B, A}}) ->
    A;
tree_peekr({tree, _, _Pr, _M, {digit, _C, _B, A}}) ->
    A;
tree_peekr({tree, _, _Pr, _M, {digit, _D, _C, _B, A}}) ->
    A.

-spec popr(finger_tree(V, X)) -> finger_tree(V, X).
popr({?MODULE, Cb, Tree}) ->
    {?MODULE, Cb, tree_popr(Cb, Tree, 1)}.

tree_popr(_Cb, {tree, _A}, _H) ->
    {tree};
tree_popr(_Cb, {tree, _, {digit, A}, {tree}, {digit, _B}}, _H) ->
    {tree, A};
tree_popr(Cb, {tree, _, {digit, A, B}, {tree}, {digit, _C}}, H) ->
    deep(Cb, {digit, A}, {tree}, {digit, B}, H);
tree_popr(Cb, {tree, _, {digit, A, B, C}, {tree}, {digit, _D}}, H) ->
    deep(Cb, {digit, A}, {tree}, {digit, B, C}, H);
tree_popr(Cb, {tree, _, {digit, A, B, C, D}, {tree}, {digit, _E}}, H) ->
    deep(Cb, {digit, A, B}, {tree}, {digit, C, D}, H);
tree_popr(Cb, {tree, _, Pr, M, {digit, _A}}, H) ->
    deep(Cb, Pr, tree_popr(Cb, M, H + 1), node_to_digit(tree_peekr(M)), H);
tree_popr(Cb, {tree, _, Pr, M, {digit, A, _B}}, H) ->
    deep(Cb, Pr, M, {digit, A}, H);
tree_popr(Cb, {tree, _, Pr, M, {digit, A, B, _C}}, H) ->
    deep(Cb, Pr, M, {digit, A, B}, H);
tree_popr(Cb, {tree, _, Pr, M, {digit, A, B, C, _D}}, H) ->
    deep(Cb, Pr, M, {digit, A, B, C}, H).

% note - can only concat trees with the same monoid...
% but won't enforce that here - comparing functions for =:= doesn't make sense
% so use with care
concat({?MODULE, Cb1, Tree1}, {?MODULE, _Cb2, Tree2}) ->
    {?MODULE, Cb1, tree_append0(Cb1, Tree1, Tree2, 1)}.

tree_append0(_Cb, {tree}, T2, _H) ->
    T2;
tree_append0(Cb, {tree, X}, T2, H) ->
    tree_pushl(Cb, X, T2, H);
tree_append0(_Cb, T1, {tree}, _H) ->
    T1;
tree_append0(Cb, T1, {tree, X}, H) ->
    tree_pushr(Cb, X, T1, H);
tree_append0(Cb, {tree, _, Pr1, M1, Sf1}, {tree, _, Pr2, M2, Sf2}, H) ->
    deep(Cb, Pr1, digit_add0(Cb, M1, Sf1, Pr2, M2, H), Sf2, H).

tree_append1(Cb, {tree}, A, T2, H) ->
    tree_pushl(Cb, A, T2, H);
tree_append1(Cb, {tree, X}, A, T2, H) ->
    T2a = tree_pushl(Cb, A, T2, H),
    tree_pushl(Cb, X, T2a, H);
tree_append1(Cb, T1, A, {tree}, H) ->
    tree_pushr(Cb, A, T1, H);
tree_append1(Cb, T1, A, {tree, X}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    tree_pushr(Cb, X, T1a, H);
tree_append1(Cb, {tree, _, Pr1, M1, Sf1}, A, {tree, _, Pr2, M2, Sf2}, H) ->
    deep(Cb, Pr1, digit_add1(Cb, M1, Sf1, A, Pr2, M2, H), Sf2, H).

tree_append2(Cb, {tree}, A, B, T2, H) ->
    T2b = tree_pushl(Cb, B, T2, H),
    tree_pushl(Cb, A, T2b, H);
tree_append2(Cb, {tree, X}, A, B, T2, H) ->
    T2b = tree_pushl(Cb, B, T2, H),
    T2a = tree_pushl(Cb, A, T2b, H),
    tree_pushl(Cb, X, T2a, H);
tree_append2(Cb, T1, A, B, {tree}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    tree_pushr(Cb, B, T1a, H);
tree_append2(Cb, T1, A, B, {tree, X}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    T1b = tree_pushr(Cb, B, T1a, H),
    tree_pushr(Cb, X, T1b, H);
tree_append2(Cb, {tree, _, Pr1, M1, Sf1}, A, B, {tree, _, Pr2, M2, Sf2}, H) ->
    deep(Cb, Pr1, digit_add2(Cb, M1, Sf1, A, B, Pr2, M2, H), Sf2, H).

tree_append3(Cb, {tree}, A, B, C, T2, H) ->
    T2c = tree_pushl(Cb, C, T2, H),
    T2b = tree_pushl(Cb, B, T2c, H),
    tree_pushl(Cb, A, T2b, H);
tree_append3(Cb, {tree, X}, A, B, C, T2, H) ->
    T2c = tree_pushl(Cb, C, T2, H),
    T2b = tree_pushl(Cb, B, T2c, H),
    T2a = tree_pushl(Cb, A, T2b, H),
    tree_pushl(Cb, X, T2a, H);
tree_append3(Cb, T1, A, B, C, {tree}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    T1b = tree_pushr(Cb, B, T1a, H),
    tree_pushr(Cb, C, T1b, H);
tree_append3(Cb, T1, A, B, C, {tree, X}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    T1b = tree_pushr(Cb, B, T1a, H),
    T1c = tree_pushr(Cb, C, T1b, H),
    tree_pushr(Cb, X, T1c, H);
tree_append3(Cb, {tree, _, Pr1, M1, Sf1}, A, B, C, {tree, _, Pr2, M2, Sf2}, H) ->
    deep(Cb, Pr1, digit_add3(Cb, M1, Sf1, A, B, C, Pr2, M2, H), Sf2, H).

tree_append4(Cb, {tree}, A, B, C, D, T2, H) ->
    T2d = tree_pushl(Cb, D, T2, H),
    T2c = tree_pushl(Cb, C, T2d, H),
    T2b = tree_pushl(Cb, B, T2c, H),
    tree_pushl(Cb, A, T2b, H);
tree_append4(Cb, {tree, X}, A, B, C, D, T2, H) ->
    T2d = tree_pushl(Cb, D, T2, H),
    T2c = tree_pushl(Cb, C, T2d, H),
    T2b = tree_pushl(Cb, B, T2c, H),
    T2a = tree_pushl(Cb, A, T2b, H),
    tree_pushl(Cb, X, T2a, H);
tree_append4(Cb, T1, A, B, C, D, {tree}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    T1b = tree_pushr(Cb, B, T1a, H),
    T1c = tree_pushr(Cb, C, T1b, H),
    tree_pushr(Cb, D, T1c, H);
tree_append4(Cb, T1, A, B, C, D, {tree, X}, H) ->
    T1a = tree_pushr(Cb, A, T1, H),
    T1b = tree_pushr(Cb, B, T1a, H),
    T1c = tree_pushr(Cb, C, T1b, H),
    T1d = tree_pushr(Cb, D, T1c, H),
    tree_pushr(Cb, X, T1d, H);
tree_append4(Cb, {tree, _, Pr1, M1, Sf1}, A, B, C, D, {tree, _, Pr2, M2, Sf2}, H) ->
    deep(Cb, Pr1, digit_add4(Cb, M1, Sf1, A, B, C, D, Pr2, M2, H), Sf2, H).

digit_add0(Cb, M1, {digit, A}, {digit, B}, M2, Ht) ->
    tree_append1(Cb, M1, node2(Cb, A, B, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A}, {digit, B, C}, M2, Ht) ->
    tree_append1(Cb, M1, node3(Cb, A, B, C, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A}, {digit, B, C, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A}, {digit, B, C, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B}, {digit, C}, M2, Ht) ->
    tree_append1(Cb, M1, node3(Cb, A, B, C, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B}, {digit, C, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B}, {digit, C, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B}, {digit, C, D, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C}, {digit, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C}, {digit, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C}, {digit, D, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C}, {digit, D, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C, D}, {digit, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C, D}, {digit, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C, D}, {digit, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add0(Cb, M1, {digit, A, B, C, D}, {digit, E, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1).

digit_add1(Cb, M1, {digit, A}, B, {digit, C}, M2, Ht) ->
    tree_append1(Cb, M1, node3(Cb, A, B, C, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A}, B, {digit, C, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A}, B, {digit, C, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A}, B, {digit, C, D, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B}, C, {digit, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B}, C, {digit, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B}, C, {digit, D, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B}, C, {digit, D, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C}, D, {digit, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C}, D, {digit, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C}, D, {digit, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C}, D, {digit, E, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C, D}, E, {digit, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C, D}, E, {digit, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C, D}, E, {digit, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add1(Cb, M1, {digit, A, B, C, D}, E, {digit, F, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1).

digit_add2(Cb, M1, {digit, A}, B, C, {digit, D}, M2, Ht) ->
    tree_append2(Cb, M1, node2(Cb, A, B, Ht), node2(Cb, C, D, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A}, B, C, {digit, D, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A}, B, C, {digit, D, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A}, B, C, {digit, D, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B}, C, D, {digit, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B}, C, D, {digit, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B}, C, D, {digit, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B}, C, D, {digit, E, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C}, D, E, {digit, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C}, D, E, {digit, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C}, D, E, {digit, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C}, D, E, {digit, F, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C, D}, E, F, {digit, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C, D}, E, F, {digit, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C, D}, E, F, {digit, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add2(Cb, M1, {digit, A, B, C, D}, E, F, {digit, G, H, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1).

digit_add3(Cb, M1, {digit, A}, B, C, D, {digit, E}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A}, B, C, D, {digit, E, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A}, B, C, D, {digit, E, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A}, B, C, D, {digit, E, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B}, C, D, E, {digit, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B}, C, D, E, {digit, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B}, C, D, E, {digit, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B}, C, D, E, {digit, F, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C}, D, E, F, {digit, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C}, D, E, F, {digit, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C}, D, E, F, {digit, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C}, D, E, F, {digit, G, H, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C, D}, E, F, G, {digit, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C, D}, E, F, G, {digit, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C, D}, E, F, G, {digit, H, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1);
digit_add3(Cb, M1, {digit, A, B, C, D}, E, F, G, {digit, H, I, J, K}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), node2(Cb, J, K, Ht), M2, Ht + 1).

digit_add4(Cb, M1, {digit, A}, B, C, D, E, {digit, F}, M2, Ht) ->
    tree_append2(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A}, B, C, D, E, {digit, F, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A}, B, C, D, E, {digit, F, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A}, B, C, D, E, {digit, F, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B}, C, D, E, F, {digit, G}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node2(Cb, D, E, Ht), node2(Cb, F, G, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B}, C, D, E, F, {digit, G, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B}, C, D, E, F, {digit, G, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B}, C, D, E, F, {digit, G, H, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C}, D, E, F, G, {digit, H}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C}, D, E, F, G, {digit, H, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C}, D, E, F, G, {digit, H, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C}, D, E, F, G, {digit, H, I, J, K}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), node2(Cb, J, K, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C, D}, E, F, G, H, {digit, I}, M2, Ht) ->
    tree_append3(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C, D}, E, F, G, H, {digit, I, J}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node2(Cb, G, H, Ht), node2(Cb, I, J, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C, D}, E, F, G, H, {digit, I, J, K}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), node2(Cb, J, K, Ht), M2, Ht + 1);
digit_add4(Cb, M1, {digit, A, B, C, D}, E, F, G, H, {digit, I, J, K, L}, M2, Ht) ->
    tree_append4(Cb, M1, node3(Cb, A, B, C, Ht), node3(Cb, D, E, F, Ht), node3(Cb, G, H, I, Ht), node3(Cb, J, K, L, Ht), M2, Ht + 1).

% Split a sequence at a point where the predicate on the accumulated measure
% changes from true to false.
-spec split(pred_fun(V), finger_tree(V, X)) ->
    {finger_tree(V, X), finger_tree(V, X)}.
split(_P, {?MODULE, {_, _, _} = Cb, {tree}}) ->
    {{?MODULE, Cb, {tree}}, {?MODULE, Cb, {tree}}};
split(P, {?MODULE, {_Meas, Id, _Op} = Cb, Tree}) ->
    case P(meas(Cb, Tree, 1)) of
        false ->
            {split, L, X, R} = tree_split(Cb, P, Id, Tree, 1),
            {{?MODULE, Cb, L}, {?MODULE, Cb, tree_pushl(Cb, X, R, 1)}};
        true ->
            {{?MODULE, Cb, Tree}, {?MODULE, Cb, {tree}}}
    end.

% fixme - implement these more efficiently
-spec takewhile(pred_fun(V), finger_tree(V, X)) -> finger_tree(V, X).
takewhile(P, FT) ->
    {L, _R} = split(P, FT),
    L.

-spec dropwhile(pred_fun(V), finger_tree(V, X)) -> finger_tree(V, X).
dropwhile(P, FT) ->
    {_L, R} = split(P, FT),
    R.

tree_split({_, _, _}, _P, _I, {tree, A}, _H) ->
    {split, {tree}, A, {tree}};
tree_split({_Meas, _Id, Op} = Cb, P, I, {tree, _, Pr, M, Sf}, H) ->
    Vpr = Op(I, meas(Cb, Pr, H)),
    Vm = op_tree(Cb, Vpr, M, H),
    PVpr = P(Vpr),
    PVm = P(Vm),
    if  not PVpr ->
            {split, L, X, R} = digit_split(Cb, P, I, Pr, H),
            {split, maybe_digit_to_tree(Cb, L, H), X, deepl(Cb, R, M, Sf, H)};
        not PVm ->
            {split, Ml, Xs, Mr} = tree_split(Cb, P, Vpr, M, H + 1),
            Vml = op_tree(Cb, Vpr, Ml, H + 1),
            {split, L, X, R} = node_split(Cb, P, Vml, Xs, H),
            {split, deepr(Cb, Pr, Ml, L, H), X, deepl(Cb, R, Mr, Sf, H)};
        true ->
            {split, L, X, R} = digit_split(Cb, P, Vm, Sf, H),
            {split, deepr(Cb, Pr, M, L, H), X, maybe_digit_to_tree(Cb, R, H)}
    end.

digit_split({_, _, _}, _P, _I, {digit, A}, _H) ->
    {split, nothing, A, nothing};
digit_split({_Meas, _Id, Op} = Cb, P, I, {digit, A, B}, H) ->
    Va = Op(I, meas(Cb, A, H - 1)),
    PVa = P(Va),
    if  not PVa -> {split, nothing, A, {just, {digit, B}}};
        true -> {split, {just, {digit, A}}, B, nothing}
    end;
digit_split({_Meas, _Id, Op} = Cb, P, I, {digit, A, B, C}, H) ->
    Va = Op(I, meas(Cb, A, H - 1)),
    Vab = Op(Va, meas(Cb, B, H - 1)),
    PVa = P(Va),
    PVab = P(Vab),
    if  not PVa -> {split, nothing, A, {just, {digit, B, C}}};
        not PVab -> {split, {just, {digit, A}}, B, {just, {digit, C}}};
        true -> {split, {just, {digit, A, B}}, C, nothing}
    end;
digit_split({_Meas, _Id, Op} = Cb, P, I, {digit, A, B, C, D}, H) ->
    Va = Op(I, meas(Cb, A, H - 1)),
    Vab = Op(Va, meas(Cb, B, H - 1)),
    Vabc = Op(Vab, meas(Cb, C, H - 1)),
    PVa = P(Va),
    PVab = P(Vab),
    PVabc = P(Vabc),
    if  not PVa -> {split, nothing, A, {just, {digit, B, C, D}}};
        not PVab -> {split, {just, {digit, A}}, B, {just, {digit, C, D}}};
        not PVabc -> {split, {just, {digit, A, B}}, C, {just, {digit, D}}};
        true -> {split, {just, {digit, A, B, C}}, D, nothing}
    end.

node_split({_Meas, _Id, Op} = Cb, P, I, {node, _, A, B}, H) ->
    Va = Op(I, meas(Cb, A, H - 1)),
    PVa = P(Va),
    if  not PVa -> {split, nothing, A, {just, {digit, B}}};
        true -> {split, {just, {digit, A}}, B, nothing}
    end;
node_split({_Meas, _Id, Op} = Cb, P, I, {node, _, A, B, C}, H) ->
    Va = Op(I, meas(Cb, A, H - 1)),
    Vab = Op(Va, meas(Cb, B, H - 1)),
    PVa = P(Va),
    PVab = P(Vab),
    if  not PVa -> {split, nothing, A, {just, {digit, B, C}}};
        not PVab -> {split, {just, {digit, A}}, B, {just, {digit, C}}};
        true -> {split, {just, {digit, A, B}}, C, nothing}
    end.

deepl(Cb, nothing, M, Sf, H) ->
    case tree_is_empty(M) of
        true -> digit_to_tree(Cb, Sf, H);
        false -> A = tree_peekl(M),
            Mp = tree_popl(Cb, M, H + 1),
            deep(Cb, node_to_digit(A), Mp, Sf, H)
    end;
deepl(Cb, {just, Pr}, M, Sf, H) ->
    deep(Cb, Pr, M, Sf, H).

deepr(Cb, Pr, M, nothing, H) ->
    case tree_is_empty(M) of
        true -> digit_to_tree(Cb, Pr, H);
        false -> A = tree_peekr(M),
            Mp = tree_popr(Cb, M, H + 1),
            deep(Cb, Pr, Mp, node_to_digit(A), H)
    end;
deepr(Cb, Pr, M, {just, Sf}, H) ->
    deep(Cb, Pr, M, Sf, H).

maybe_digit_to_tree(_Cb, nothing, _H) ->
    {tree};
maybe_digit_to_tree(Cb, {just, Digit}, H) ->
    digit_to_tree(Cb, Digit, H).

digit_to_tree(_Cb, {digit, A}, _H) ->
    {tree, A};
digit_to_tree(Cb, {digit, A, B}, H) ->
    deep(Cb, {digit, A}, {tree}, {digit, B}, H);
digit_to_tree(Cb, {digit, A, B, C}, H) ->
    deep(Cb, {digit, A, B}, {tree}, {digit, C}, H);
digit_to_tree(Cb, {digit, A, B, C, D}, H) ->
    deep(Cb, {digit, A, B}, {tree}, {digit, C, D}, H).

-spec foldl(fun((X, A) -> A), A, finger_tree(_V, X)) -> A.
foldl(Fun, Acc, {?MODULE, _Cb, Tree}) ->
    tree_foldl(Fun, Acc, Tree, 1).

tree_foldl(_Fun, Acc, {tree}, _H) ->
    Acc;
tree_foldl(Fun, Acc, {tree, A}, 1) ->
    Fun(A, Acc);
tree_foldl(Fun, Acc, {tree, A}, H) ->
    node_foldl(Fun, Acc, A, H - 1);
tree_foldl(Fun, Acc0, {tree, _, Pr, M, Sf}, H) ->
    Acc1 = digit_foldl(Fun, Acc0, Pr, H),
    Acc2 = tree_foldl(Fun, Acc1, M, H + 1),
    digit_foldl(Fun, Acc2, Sf, H).

-spec foldr(fun((X, A) -> A), A, finger_tree(_V, X)) -> A.
foldr(Fun, Acc, {?MODULE, _Cb, Tree}) ->
    tree_foldr(Fun, Acc, Tree, 1).

tree_foldr(_Fun, Acc, {tree}, _H) ->
    Acc;
tree_foldr(Fun, Acc, {tree, A}, 1) ->
    Fun(A, Acc);
tree_foldr(Fun, Acc, {tree, A}, H) ->
    node_foldr(Fun, Acc, A, H - 1);
tree_foldr(Fun, Acc0, {tree, _, Pr, M, Sf}, H) ->
    Acc1 = digit_foldr(Fun, Acc0, Sf, H),
    Acc2 = tree_foldr(Fun, Acc1, M, H + 1),
    digit_foldr(Fun, Acc2, Pr, H).

digit_foldl(Fun, Acc, {digit, A}, 1) ->
    Fun(A, Acc);
digit_foldl(Fun, Acc, {digit, A, B}, 1) ->
    Fun(B, Fun(A, Acc));
digit_foldl(Fun, Acc, {digit, A, B, C}, 1) ->
    Fun(C, Fun(B, Fun(A, Acc)));
digit_foldl(Fun, Acc, {digit, A, B, C, D}, 1) ->
    Fun(D, Fun(C, Fun(B, Fun(A, Acc))));
digit_foldl(Fun, Acc, {digit, A}, H) ->
    node_foldl(Fun, Acc, A, H - 1);
digit_foldl(Fun, Acc0, {digit, A, B}, H) ->
    Acc1 = node_foldl(Fun, Acc0, A, H - 1),
    node_foldl(Fun, Acc1, B, H - 1);
digit_foldl(Fun, Acc0, {digit, A, B, C}, H) ->
    Acc1 = node_foldl(Fun, Acc0, A, H - 1),
    Acc2 = node_foldl(Fun, Acc1, B, H - 1),
    node_foldl(Fun, Acc2, C, H - 1);
digit_foldl(Fun, Acc0, {digit, A, B, C, D}, H) ->
    Acc1 = node_foldl(Fun, Acc0, A, H - 1),
    Acc2 = node_foldl(Fun, Acc1, B, H - 1),
    Acc3 = node_foldl(Fun, Acc2, C, H - 1),
    node_foldl(Fun, Acc3, D, H - 1).

node_foldl(Fun, Acc, {node, _, A, B}, 1) ->
    Fun(B, Fun(A, Acc));
node_foldl(Fun, Acc, {node, _, A, B, C}, 1) ->
    Fun(C, Fun(B, Fun(A, Acc)));
node_foldl(Fun, Acc0, {node, _, A, B}, H) ->
    Acc1 = node_foldl(Fun, Acc0, A, H - 1),
    node_foldl(Fun, Acc1, B, H - 1);
node_foldl(Fun, Acc0, {node, _, A, B, C}, H) ->
    Acc1 = node_foldl(Fun, Acc0, A, H - 1),
    Acc2 = node_foldl(Fun, Acc1, B, H - 1),
    node_foldl(Fun, Acc2, C, H - 1).

digit_foldr(Fun, Acc, {digit, A}, 1) ->
    Fun(A, Acc);
digit_foldr(Fun, Acc, {digit, A, B}, 1) ->
    Fun(A, Fun(B, Acc));
digit_foldr(Fun, Acc, {digit, A, B, C}, 1) ->
    Fun(A, Fun(B, Fun(C, Acc)));
digit_foldr(Fun, Acc, {digit, A, B, C, D}, 1) ->
    Fun(A, Fun(B, Fun(C, Fun(D, Acc))));
digit_foldr(Fun, Acc, {digit, A}, H) ->
    node_foldr(Fun, Acc, A, H - 1);
digit_foldr(Fun, Acc0, {digit, A, B}, H) ->
    Acc1 = node_foldr(Fun, Acc0, B, H - 1),
    node_foldr(Fun, Acc1, A, H - 1);
digit_foldr(Fun, Acc0, {digit, A, B, C}, H) ->
    Acc1 = node_foldr(Fun, Acc0, C, H - 1),
    Acc2 = node_foldr(Fun, Acc1, B, H - 1),
    node_foldr(Fun, Acc2, A, H - 1);
digit_foldr(Fun, Acc0, {digit, A, B, C, D}, H) ->
    Acc1 = node_foldr(Fun, Acc0, D, H - 1),
    Acc2 = node_foldr(Fun, Acc1, C, H - 1),
    Acc3 = node_foldr(Fun, Acc2, B, H - 1),
    node_foldr(Fun, Acc3, A, H - 1).

node_foldr(Fun, Acc, {node, _, A, B}, 1) ->
    Fun(A, Fun(B, Acc));
node_foldr(Fun, Acc, {node, _, A, B, C}, 1) ->
    Fun(A, Fun(B, Fun(C, Acc)));
node_foldr(Fun, Acc0, {node, _, A, B}, H) ->
    Acc1 = node_foldr(Fun, Acc0, B, H - 1),
    node_foldr(Fun, Acc1, A, H - 1);
node_foldr(Fun, Acc0, {node, _, A, B, C}, H) ->
    Acc1 = node_foldr(Fun, Acc0, C, H - 1),
    Acc2 = node_foldr(Fun, Acc1, B, H - 1),
    node_foldr(Fun, Acc2, A, H - 1).

to_list({?MODULE, _Cb, Tree}) ->
    tree_foldr(fun(X, L) -> [X | L] end, [], Tree, 1).
