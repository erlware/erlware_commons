-module(deque_eqc).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

non_empty_deque(G) ->
    ?SIZED(Size, well_defined(non_empty_deque(Size, G))).
non_empty_deque(N, G) ->
    ?SUCHTHAT(D, deque(N, G), deque:is_empty(eval(D)) == false).

deque(G) ->
    ?SIZED(Size, well_defined(deque(Size, G))).

deque(0, G) ->
    oneof([{call, deque, new, []},
            {call, lists, foldl, [return(fun deque:pushr/2),
                    return(deque:new()), list(G)]}]);
deque(N, G) ->
    frequency([{5, deque(0, G)},
            {3, ?LAZY(?LETSHRINK([D], [deque(N-1, G)],
                        {call, deque, pushl, [G, D]}))},
            {3, ?LAZY(?LETSHRINK([D], [deque(N-1, G)],
                        {call, deque, pushr, [G, D]}))},
            {3, ?LAZY(?LETSHRINK([D1, D2],
                        [deque(N div 2, G), deque(N div 2, G)],
                        {call, deque, concat, [D1, D2]}))},
            {1, ?LAZY(?LETSHRINK([D], [non_empty_deque(N-1, G)],
                        {call, deque, popl, [D]}))},
            {1, ?LAZY(?LETSHRINK([D], [non_empty_deque(N-1, G)],
                        {call, deque, popr, [D]}))}]).

model(D) ->
    model(D, []).

model(D, L) ->
    case deque:is_empty(D) of
        true -> L;
        false -> model(deque:popr(D), [deque:peekr(D) | L])
    end.

mpushl(X, L) ->
    [X | L].

mpeekl([H | _T]) ->
    H.

mpopl([_H | T]) ->
    T.

mpushr(X, L) ->
    L ++ [X].

mpeekr([X]) ->
    X;
mpeekr([_H | T]) ->
    mpeekr(T).

mpopr([_]) ->
    [];
mpopr([H | T]) ->
    [H | mpopr(T)].

mconcat(L1, L2) ->
    L1 ++ L2.

msize(L) ->
    length(L).

prop_pushl() -> prop_pushl(int()).
prop_pushl(G) ->
    ?FORALL({E, D}, {G, deque(G)},
        mpushl(E, model(eval(D))) == model(deque:pushl(E, eval(D)))).

prop_peekl() -> prop_peekl(int()).
prop_peekl(G) ->
    ?FORALL(D, non_empty_deque(G),
        mpeekl(model(eval(D))) == deque:peekl(eval(D))).

prop_popl() -> prop_popl(int()).
prop_popl(G) ->
    ?FORALL(D, non_empty_deque(G),
        mpopl(model(eval(D))) == model(deque:popl(eval(D)))).

prop_pushr() -> prop_pushr(int()).
prop_pushr(G) ->
    ?FORALL({E, D}, {G, deque(G)},
        mpushr(E, model(eval(D))) == model(deque:pushr(E, eval(D)))).

prop_peekr() -> prop_peekr(int()).
prop_peekr(G) ->
    ?FORALL(D, non_empty_deque(G),
        mpeekr(model(eval(D))) == deque:peekr(eval(D))).

prop_popr() -> prop_popr(int()).
prop_popr(G) ->
    ?FORALL(D, non_empty_deque(G),
        mpopr(model(eval(D))) == model(deque:popr(eval(D)))).

prop_concat() -> prop_concat(int()).
prop_concat(G) ->
    ?FORALL({D1, D2}, {deque(G), deque(G)},
        mconcat(model(eval(D1)), model(eval(D2))) ==
        model(deque:concat(eval(D1), eval(D2)))).

prop_size() -> prop_size(int()).
prop_size(G) ->
    ?FORALL(D, deque(G),
        msize(model(eval(D))) == deque:size(eval(D))).

list_cons(X, L) ->
    [X | L].

prop_foldl() -> prop_foldl(int()).
prop_foldl(G) ->
    ?FORALL(D, deque(G),
        lists:reverse(model(eval(D))) =:=
        deque:foldl(fun list_cons/2, [], eval(D))).

prop_foldr() -> prop_foldr(int()).
prop_foldr(G) ->
    ?FORALL(D, deque(G),
        model(eval(D)) =:= deque:foldr(fun list_cons/2, [], eval(D))).

prop_from_list() -> prop_from_list(int()).
prop_from_list(G) ->
    ?FORALL(L, list(G),
        L == model(deque:from_list(L))).

prop_to_list() -> prop_to_list(int()).
prop_to_list(G) ->
    ?FORALL(D, deque(G),
        model(eval(D)) == deque:to_list(eval(D))).

prop_build_unbuild() -> prop_build_unbuild(int()).
prop_build_unbuild(G) ->
    ?FORALL(L, list(G),
        L =:= deque:foldl(fun list_cons/2, [],
            lists:foldl(fun deque:pushl/2, deque:new(), L))).

prop_build_unbuild2() -> prop_build_unbuild2(int()).
prop_build_unbuild2(G) ->
    ?FORALL(L, list(G),
        L =:= deque:foldr(fun list_cons/2, [],
            lists:foldl(fun deque:pushr/2, deque:new(), L))).

prop_build_unbuild3() -> prop_build_unbuild3(int()).
prop_build_unbuild3(G) ->
    ?FORALL(L, list(G),
        L =:= model(lists:foldl(fun deque:pushr/2, deque:new(), L))).

prop_build_unbuild4() -> prop_build_unbuild4(int()).
prop_build_unbuild4(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(deque:concat(
                lists:foldr(fun deque:pushl/2, deque:new(), L1),
                lists:foldr(fun deque:pushl/2, deque:new(), L2)))).

prop_build_unbuild5() -> prop_build_unbuild5(int()).
prop_build_unbuild5(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(deque:concat(
                lists:foldr(fun deque:pushl/2, deque:new(), L1),
                lists:foldl(fun deque:pushr/2, deque:new(), L2)))).

prop_build_unbuild6() -> prop_build_unbuild6(int()).
prop_build_unbuild6(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(deque:concat(
                lists:foldl(fun deque:pushr/2, deque:new(), L1),
                lists:foldr(fun deque:pushl/2, deque:new(), L2)))).

prop_build_unbuild7() -> prop_build_unbuild7(int()).
prop_build_unbuild7(G) ->
    ?FORALL({L1, L2}, {list(G), list(G)},
        L1 ++ L2 =:= model(deque:concat(
                lists:foldl(fun deque:pushr/2, deque:new(), L1),
                lists:foldl(fun deque:pushr/2, deque:new(), L2)))).
