-module(mock).

-export([new_dictionary/0]).

new_dictionary() ->
    meck:new(ec_dictionary_proper),
    meck:expect(ec_dictionary_proper, dictionary, fun() ->
							  proper_types:union([ec_dict])
						  end).
							  
