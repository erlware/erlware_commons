-module(prio_queue_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

non_empty_prio_queue(F, G) ->
    ?SIZED(Size, well_defined(non_empty_prio_queue(Size, F, G))).
non_empty_prio_queue(N, F, G) ->
    ?SUCHTHAT(PQ, prio_queue(N, F, G), prio_queue:is_empty(eval(PQ)) == false).

prio_queue(F, G) ->
    ?SIZED(Size, well_defined(prio_queue(Size, F, G))).

prio_queue(0, F, G) ->
    frequency([{1, {call, prio_queue, new, [return(F)]}},
            {4, ?LAZY({call, lists, foldl, [return(fun prio_queue:push/2),
                            {call, prio_queue, new, [return(F)]}, list(G)]})}]);
prio_queue(N, F, G) ->
    frequency([{2, prio_queue(0, F, G)},
            {4, ?LAZY(?LETSHRINK([PQ], [prio_queue(N-1, F, G)],
                        {call, prio_queue, push, [G, PQ]}))},
            {1, ?LAZY(?LETSHRINK([PQ], [non_empty_prio_queue(N-1, F, G)],
                        {call, erlang, element, [2,
                                {call, prio_queue, pop, [PQ]}]}))}]).

model({prio_queue, CmpFun, FT}) ->
    {CmpFun, lists:sort(finger_tree:foldl(fun(X, L) -> [X | L] end, [], FT))}.

mis_empty({_, []}) ->
    true;
mis_empty({_, [_ | _]}) ->
    false.

mpush(X, {F, L}) ->
    {F, lists:sort([X | L])}.

mpop({F, [Y | L]}) ->
    X = lists:foldl(fun(E, M) ->
                case F(E, M) of
                    true -> E;
                    _ -> M
                end end, Y, L),
    {X, {F, lists:sort(lists:delete(X, [Y | L]))}}.

prop_push() -> prop_push(fun(X, Y) -> X > Y end, int()).
prop_push(F, G) ->
    ?FORALL({E, PQ}, {G, prio_queue(F, G)},
        mpush(E, model(eval(PQ))) == model(prio_queue:push(E, eval(PQ)))).

prop_pop() -> prop_pop(fun(X, Y) -> X > Y end, int()).
prop_pop(F, G) ->
    ?FORALL(PQ, non_empty_prio_queue(F, G),
        begin
            {X, PQ2} = prio_queue:pop(eval(PQ)),
            mpop(model(eval(PQ))) == {X, model(PQ2)}
        end).
