%% compile with
%% erl -pz ebin --make
%% start test with
%%   erl -pz ebin -pz test
%%   proper:module(ec_dictionary_proper).
-module(ec_dictionary_proper).

-ifdef(DEV_ONLY).

-export([my_dict/0, dict/1, sym_dict/0, sym_dict/1, gb_tree/0, gb_tree/1, sym_dict2/0]).

-include_lib("proper/include/proper.hrl").


%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_size_increases_with_new_key() ->
    ?FORALL({Dict,K}, {sym_dict(),integer()},
            begin
                Size = ec_dictionary:size(Dict),
                case ec_dictionary:has_key(K,Dict) of
                    true ->
                        Size == ec_dictionary:size(ec_dictionary:add(K,0,Dict));
                    false ->
                        (Size + 1) == ec_dictionary:size(ec_dictionary:add(K,0,Dict))
                end
            end).

prop_size_decrease_when_removing() ->
    ?FORALL({Dict,K}, {sym_dict(),integer()},
            begin
                Size = ec_dictionary:size(Dict),
                case ec_dictionary:has_key(K,Dict) of
                    false ->
                        Size == ec_dictionary:size(ec_dictionary:remove(K,Dict));
                    true ->
                        (Size - 1) == ec_dictionary:size(ec_dictionary:remove(K,Dict))
                end
            end).

prop_get_after_add_returns_correct_value() ->
    ?FORALL({Dict,K,V}, {sym_dict(),key(),value()},
            begin
                try ec_dictionary:get(K,ec_dictionary:add(K,V,Dict)) of
                    V ->
                        true;
                    _ ->
                        false
                catch
                    _:_ ->
                        false
                end
            end).

prop_get_default_returns_correct_value() ->
    ?FORALL({Dict,K1,K2,V,Default},
            {sym_dict(),key(),key(),value(),value()},
            begin
                NewDict = ec_dictionary:add(K1,V, Dict),
                %% In the unlikely event that keys that are the same
                %% are generated
                case ec_dictionary:has_key(K2, NewDict) of
                    true ->
                        true;
                    false ->
                        ec_dictionary:get(K2, Default, NewDict) == Default
                end
            end).


prop_add_does_not_change_values_for_other_keys() ->
    ?FORALL({Dict,K,V},  {sym_dict(),key(),value()},
            begin
                Keys = ec_dictionary:keys(Dict),
                ?IMPLIES(not lists:member(K,Keys),
                         begin
                             Dict2 = ec_dictionary:add(K,V,Dict),
                             try lists:all(fun(B) -> B end,
                                           [ ec_dictionary:get(Ka,Dict) ==
                                                 ec_dictionary:get(Ka,Dict2) ||
                                               Ka <- Keys ]) of
                                 Bool -> Bool
                             catch
                                 throw:not_found -> true
                             end
                         end)
            end).



prop_key_is_present_after_add() ->
    ?FORALL({Dict,K,V}, {sym_dict(),integer(),integer()},
            begin
                ec_dictionary:has_key(K,ec_dictionary:add(K,V,Dict))        end).

prop_value_is_present_after_add() ->
    ?FORALL({Dict,K,V}, {sym_dict(),integer(),integer()},
            begin
                ec_dictionary:has_value(V,ec_dictionary:add(K,V,Dict))
            end).

prop_to_list_matches_get() ->
    ?FORALL(Dict,sym_dict(),
            begin
                %% Dict = eval(SymDict),
                %% io:format("SymDict: ~p~n",[proper_symb:symbolic_seq(SymDict)]),
                ToList = ec_dictionary:to_list(Dict),
                %% io:format("ToList:~p~n",[ToList]),
                GetList =
                    try [ {K,ec_dictionary:get(K,Dict)} || {K,_V} <- ToList ] of
                        List -> List
                    catch
                        throw:not_found -> key_not_found
                    end,
                %% io:format("~p == ~p~n",[ToList,GetList]),
                lists:sort(ToList) == lists:sort(GetList)
            end).

prop_value_changes_after_update() ->
    ?FORALL({Dict, K1, V1, V2},
            {sym_dict(),
             key(), value(), value()},
            begin
                Dict1 = ec_dictionary:add(K1, V1, Dict),
                Dict2 = ec_dictionary:add(K1, V2, Dict1),
                V1 == ec_dictionary:get(K1, Dict1) andalso
                    V2 == ec_dictionary:get(K1, Dict2)
            end).

prop_remove_removes_only_one_key() ->
    ?FORALL({Dict,K},
            {sym_dict(),key()},
            begin
                {KeyGone,Dict2} = case ec_dictionary:has_key(K,Dict) of
                                      true ->
                                          D2 = ec_dictionary:remove(K,Dict),
                                          {ec_dictionary:has_key(K,D2) == false,
                                           D2};
                                      false ->
                                          {true,ec_dictionary:remove(K,Dict)}
                                  end,
                OtherEntries = [ KV || {K1,_} = KV <- ec_dictionary:to_list(Dict),
                                       K1 /= K ],
                KeyGone andalso
                    lists:sort(OtherEntries) == lists:sort(ec_dictionary:to_list(Dict2))
            end).

prop_from_list() ->
    ?FORALL({Dict,DictType},
            {sym_dict(),dictionary()},
            begin
                List = ec_dictionary:to_list(Dict),
                D2 = ec_dictionary:from_list(DictType,List),
                List2 = ec_dictionary:to_list(D2),
                lists:sort(List) == lists:sort(List2)
            end).


%%-----------------------------------------------------------------------------
%% Generators
%%-----------------------------------------------------------------------------

key() -> union([integer(),atom()]).

value() -> union([integer(),atom(),binary(),boolean(),string()]).


my_dict() ->
    ?SIZED(N,dict(N)).


dict(0) ->
    ec_dictionary:new(ec_gb_trees);
dict(N) ->
    ?LET(D,dict(N-1),
         frequency([
                    {1, dict(0)},
                    {3, ec_dictionary:remove(integer(),D)},
                    {6, ec_dictionary:add(integer(),integer(),D)}
                  ])).

sym_dict() ->
    ?SIZED(N,sym_dict(N)).

%% This symbolic generator will create a random instance of a ec_dictionary
%% that will be used in the properties.
sym_dict(0) ->
    ?LET(Dict,dictionary(),
         {'$call',ec_dictionary,new,[Dict]});
sym_dict(N) ->
    ?LAZY(
       frequency([
                  {1, sym_dict(0)},
                  {3, {'$call',ec_dictionary,remove,[key(),sym_dict(N-1)]}},
                  {6, {'$call',ec_dictionary,add,[value(),value(),sym_dict(N-1)]}}
                 ])
      ).

dictionary() ->
    union([ec_gb_trees,ec_assoc_list,ec_dict,ec_orddict]).

sym_dict2() ->
    ?SIZED(N,sym_dict2(N)).

sym_dict2(0) ->
    {call,ec_dictionary,new,[ec_gb_trees]};
sym_dict2(N) ->
    D = dict(N-1),
    frequency([
               {1, {call,ec_dictionary,remove,[integer(),D]}},
               {2, {call,ec_dictionary,add,[integer(),integer(),D]}}
               ]).


%% For the tutorial.
gb_tree() ->
    ?SIZED(N,gb_tree(N)).

gb_tree(0) ->
    gb_trees:empty();
gb_tree(N) ->
    gb_trees:enter(key(),value(),gb_tree(N-1)).

-endif.
