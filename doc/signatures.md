Signatures
==========

It often occurs in coding that we need a library, a set of
functionalities. Often there are several algorithms that could provide
each of these functionalities. However, the code that uses it, either doesn't
care about the individual algorithm or wishes to delegate choosing
that algorithm to some higher level. Let's take the concrete example of
dictionaries. A dictionary provides the ability to access a value via
a key (other things as well but primarily this). There are may ways to
implement a dictionary. Just a few are:

* [Associative Arrays](http://en.wikipedia.org/wiki/Associative_array)
* [Binary Trees](http://en.wikipedia.org/wiki/Binary_tree)
* [Hash Tables](http://en.wikipedia.org/wiki/Hash_table#Performance_analysis)
* [Skip Lists](http://en.wikipedia.org/wiki/Skip_list)
* Many, many more ....

Each of these approaches has their own performance characteristics,
memory footprints etc. For example, a table of size n with open
addressing has no collisions and holds up to n elements, with a single
comparison for successful lookup, and a table of size n with chaining
and k keys has the minimum max(0, k-n) collisions and O(1 + k/n)
comparisons for lookup. While for skip lists the performance
characteristics are about as good as that of randomly-built binary
search trees - namely (O log n). So the choice of which to select
depends very much on memory available, insert/read characteristics,
etc. So delegating the choice to a single point in your code is a very
good idea. Unfortunately, in Erlang that's so easy to do at the moment.

Other languages, have built in support for this
functionality. [Java](http://en.wikipedia.org/wiki/Java_(programming_language))
has
[Interfaces](http://download.oracle.com/javase/tutorial/java/IandI/createinterface.html),
[SML](http://en.wikipedia.org/wiki/Standard_ML) has
[Signatures](http://en.wikipedia.org/wiki/Standard_ML#Module_system).
Erlang, though, doesn't currently support this model, at least not
directly. There are a few ways you can approximate it. One way is to
pass the Module name to the calling functions along with the data that
it is going to be called on.

	```erlang
	add(ModuleToUse, Key, Value, DictData) ->
		ModuleToUse:add(Key, Value, DictData).
	```

This works, and you can vary how you want to pass the data. For
example, you could easily use a tuple to contain the data. That is,
you could pass in `{ModuleToUse, DictData}` and that would make it a
bit cleaner. 

	```erlang
    add(Key, Value, {ModuleToUse, DictData}) ->
        ModuleToUse:add(Key, Value, DictData).
	```

Either way, there are a few problems with this approach. One of the
biggest is that you lose code locality, by looking at this bit of code
you don't know what `ModuleToUse` is at all. You would need to follow
the call chain up to figure out what it is. Also it may not be obvious
what is actually happening. The fact that `ModuleToUse` is a variable
name obscures the code making it harder to understand. The other big
problem is that the tools provided with Erlang can't help find
mistakes that you might have made. Tools like
[Xref](http://www.erlang.org/doc/man/xref.html) and
[Dialyzer](http://www.erlang.org/doc/man/dialyzer.html) have just as
hard a time figuring out the what `ModuleToUse` is pointing to as you
do. So they can't give you warnings about potential problems. In fact
someone could inadvertently pass an unexpected function name as
`ModuleToUse` and you would never get any warnings, just an exception
at run time.

Fortunately, Erlang is a pretty flexible language so we can use a
similar approach with a few adjustments to give us the best of both
worlds. Both the flexibility of ignoring a specific implementation
and keeping all the nice locality we get by using an explicit module
name.

So what we actually want to do is something mole like this:

    ```erlang
    add(Key, Value, DictData) ->
        dictionary:add(Key, Value, DictData).
	```

Doing this we retain the locality. We can easily look up the
`dictionary` Module. We immediately have a good idea what a
`dictionary` actually is and we know what functions we are
calling. Also, all the tools know what a `dictionary` is as well and
how to check that your code is calling it correctly. For all of these
reasons, this is a much better approach to the problem. This is what
*Signatures* are all about.

Signatures
----------

How do we actually do this in Erlang now that Erlang is missing what Java, SML and friends have built in? 

The first thing we need to do is to define
a [Behaviour](http://metajack.im/2008/10/29/custom-behaviors-in-erlang/)
for our functionality. To continue our example we will define a
Behaviour for dictionaries. That Behaviour looks like this:

    ```erlang
    -module(ec_dictionary).

    -export([behaviour_info/1]).

    behaviour_info(callbacks) ->
        [{new, 0},
         {has_key, 2},
         {get, 2},
         {add, 3},
         {remove, 2},
         {has_value, 2},
         {size, 1},
         {to_list, 1},
         {from_list, 1},
         {keys, 1}];
    behaviour_info(_) ->
        undefined.
	```


So we have our Behaviour now. Unfortunately, this doesn't give us much
yet. It will make sure that any dictionaries we write will have all
the functions they need to have, but it won't help us actually use the
dictionaries in an abstract way in our code. To do that we need to add
a bit of functionality. We do that by actually implementing our own
behaviour, starting with `new/1`.

    ```erlang
    %% @doc create a new dictionary object from the specified module. The
    %% module should implement the dictionary behaviour.
    %%
    %% @param ModuleName The module name.
    -spec new(module()) -> dictionary(_K, _V).
    new(ModuleName) when is_atom(ModuleName) ->
        #dict_t{callback = ModuleName, data = ModuleName:new()}.
	```

This code creates a new dictionary for us. Or to be more specific it
actually creates a new dictionary Signature record, that will be used
subsequently in other calls. This might look a bit familiar from our
previous less optimal approach. We have both the module name and the
data in the record. We call the module name named in
`ModuleName` to create the initial data. We then construct the record
and return that record to the caller and we have a new
dictionary. What about the other functions, the ones that don't create
a dictionary but make use of it. Let's take a look at the
implementations of two kinds of functions, one that updates the
dictionary and another that just retrieves data.

The first we will look at is the one that updates the dictionary by
adding a value.

    ```erlang
    %% @doc add a new value to the existing dictionary. Return a new
    %% dictionary containing the value.
    %%
    %% @param Dict the dictionary object to add too
    %% @param Key the key to add
    %% @param Value the value to add
    -spec add(key(K), value(V), dictionary(K, V)) -> dictionary(K, V).
    add(Key, Value, #dict_t{callback = Mod, data = Data} = Dict) ->
        Dict#dict_t{data = Mod:add(Key, Value, Data)}.
	```

There are two key things here. 

1. The dictionary is deconstructed so we can get access to the data
and the callback module.
1. We modify the dictionary record we the new data and return that
modified record.

This is the same approach that you will use for any Signature that
updates data. As a side note, notice that we are calling the concrete
implementation to do the work itself.

Now lets do a data retrieval function. In this case, the `get` function
of the dictionary Signature.

    ```erlang
    %% @doc given a key return that key from the dictionary. If the key is
    %% not found throw a 'not_found' exception.
    %%
    %% @param Dict The dictionary object to return the value from
    %% @param Key The key requested
    %% @throws not_found when the key does not exist
    -spec get(key(K), dictionary(K, V)) -> value(V).
    get(Key, #dict_t{callback = Mod, data = Data}) ->
        Mod:get(Key, Data).
	```

In this case, you can see a very similar approach to deconstructing
the dict record. We still need to pull out the callback module and the
data itself and call the concrete implementation of the algorithm. In
this case, we return the data returned from the call, not the record
itself.

That is really all you need to define a Signature. There is a complete
implementation in
[erlware_commons/ec_dictionary](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_dictionary.erl).

Using Signatures
----------------

It's a good idea to work through an example so we have a bit better
idea of how to use these Signatures. If you are like me, you probably
have some questions about what kind of performance burden this places
on the code. At the very least we have an additional function call
along with the record deconstruction. This must add some overhead. So
lets write a little timing test, so we can get a good idea of how much
this is all costing us.

In general, there are two kinds of concrete implementations for
Signatures. The first is a native implementation, the second is a
wrapper.

### Native Signature Implementations

A Native Signature Implementation is just that, a module that
implements the Behaviour defined by a Signature directly. For most
user defined Signatures this is going to be the norm. In our current
example, the
[erlware_commons/ec_rbdict](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_rbdict.erl)
module is the best example of a Native Signature Implementation. It
implements the ec_dictionary module directly.

### Signature Wrappers

A Signature Wrapper is a module that wraps another module. Its
purpose is to help a preexisting module implement the Behaviour
defined by a Signature. A good example of this in our current example
is the
[erlware_commons/ec_dict](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_dict.erl)
module. It implements the `ec_dictionary` Behaviour, but all the
functionality is provided by the
[stdlib/dict](http://www.erlang.org/doc/man/dict.html) module
itself. Let's take a look at one example to see how this is done.

We will take a look at one of the functions we have already seen. The
`get` function in `ec_dictionary` doesn't have quite the same
semantics as any of the functions in the `dict` module. So a bit of
translation needs to be done. We do that in the `ec_dict:get/2` function.

    ```erlang
    -spec get(ec_dictionary:key(K), Object::dictionary(K, V)) ->
               ec_dictionary:value(V).
    get(Key, Data) ->
        case dict:find(Key, Data) of
	    {ok, Value} ->
	        Value;
	     error ->
	        throw(not_found)
        end.
	```

So the `ec_dict` module's purpose for existence is to help the
preexisting `dict` module implement the Behaviour defined by the
Signature.


Why do we bring this up here? Because we are going to be looking at
timings, and Signature Wrappers add an extra level of indirection to
the mix and that adds a bit of additional overhead.

### Creating the Timing Module

We are going to be creating timings for both Native Signature
Implementations and Signature Wrappers.

Let's get started by looking at some helper functions. We want
dictionaries to have a bit of data in them. So to that end we will
create a couple of functions that create dictionaries for each type we
want to test. The first we want to time is the Signature Wrapper, so
`dict` vs `ec_dict` called as a Signature.

    ```erlang
    create_dict() ->
    lists:foldl(fun(El, Dict) ->
			dict:store(El, El, Dict)
		end, dict:new(),
		lists:seq(1,100)).
	```

The only thing we do here is create a sequence of numbers 1 to 100,
and then add each of those to the `dict` as an entry. We aren't too
worried about replicating real data in the dictionary. We care about
timing the function call overhead of Signatures, not the performance
of the dictionaries themselves.

We need to create a similar function for our Signature based
dictionary `ec_dict`.

    ```erlang
    create_dictionary(Type) ->
    lists:foldl(fun(El, Dict) ->
			ec_dictionary:add(El, El, Dict)
		end,
		ec_dictionary:new(Type),
		lists:seq(1,100)).
	```

Here we actually create everything using the Signature. So we don't
need one function for each type. We can have one function that can
create anything that implements the Signature. That is the magic of
Signatures. Otherwise, this does the exact same thing as the dictionary
given by `create_dict/0`.

We are going to use two function calls in our timing. One that updates
data and one that returns data, just to get good coverage. For our
dictionaries we are going to use the `size` function as well as
the `add` function.

    ```erlang
    time_direct_vs_signature_dict() ->
        io:format("Timing dict~n"),
        Dict = create_dict(),
        test_avg(fun() ->
	             dict:size(dict:store(some_key, some_value, Dict))
	         end,
	         1000000),
        io:format("Timing ec_dict implementation of ec_dictionary~n"),
        time_dict_type(ec_dict).
	```

The `test_avg` function runs the provided function the number of times
specified in the second argument and collects timing information. We
are going to run these one million times to get a good average (it's
fast so it doesn't take long). You can see in the anonymous
function that we directly call `dict:size/1` and `dict:store/3` to perform
the test. However, because we are in the wonderful world of Signatures
we don't have to hard code the calls for the Signature
implementations. Lets take a look at the `time_dict_type` function.


    ```erlang
    time_dict_type(Type) ->
        io:format("Testing ~p~n", [Type]),
        Dict = create_dictionary(Type),
        test_avg(fun() ->
		     ec_dictionary:size(ec_dictionary:add(some_key, some_value, Dict))
	         end,
	         1000000).
	```

As you can see we take the type as an argument (we need it for `dict`
creation) and call our create function. Then we run the same timings
that we did for `ec_dict`. In this case though, the type of dictionary
is never specified, we only ever call ec_dictionary, so this test will
work for anything that implements that Signature.

#### `dict` vs `ec_dict` Results

So we have our tests, what was the result. Well on my laptop this is
what it looked like.

    ```sh
    Erlang R14B01 (erts-5.8.2) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)

    1> ec_timing:time_direct_vs_signature_dict().
    Timing dict
    Range: 2 - 5621 mics
    Median: 3 mics
    Average: 3 mics
    Timing ec_dict implementation of ec_dictionary
    Testing ec_dict
    Range: 3 - 6097 mics
    Median: 3 mics
    Average: 4 mics
    2>
	```

So for the direct `dict` call, we average about 3 mics per call, while
for the Signature Wrapper we average around 4. That's a 25% cost for
Signature Wrappers in this example, for a very small number of
calls. Depending on what you are doing that is going to be greater or
lesser. In any case, we can see that there is some cost associated
with the Signature Wrapper Implementations.

What about native Signatures though? Lets take a look at
`ec_rbdict`. The `ec_rbdict` also implements the `ec_dictionary`
Signature, but it is not a Signature Wrapper. It is a native
implementation of the Signature. To use `ec_rbdict` directly we have
to create a creation helper just like we did for dict.

    ```erlang
    create_rbdict() ->
    lists:foldl(fun(El, Dict) ->
			ec_rbdict:add(El, El, Dict)
		end, ec_rbdict:new(),
		lists:seq(1,100)).
	```

This is exactly the same as `create_dict` with the exception that dict
is replaced by `ec_rbdict`.

The timing function itself looks very similar as well. Again notice
that we have to hard code the concrete name for the concrete
implementation, but we don't for the `ec_dictionary` test.

    ```erlang
    time_direct_vs_signature_rbdict() ->
        io:format("Timing rbdict~n"),
        Dict = create_rbdict(),
        test_avg(fun() ->
	 	     ec_rbdict:size(ec_rbdict:add(some_key, some_value, Dict))
	         end,
	         1000000),
       io:format("Timing ec_dict implementation of ec_dictionary~n"),
       time_dict_type(ec_rbdict).
   ```

And there we have our test. What do the results look like?

#### `ec_rbdict` vs `ec_rbdict` as an `ec_dictionary` Results

The main thing we are timing here is the additional cost of the
dictionary Signature itself. Keep that in mind as we look at the
results.

    ```sh
    Erlang R14B01 (erts-5.8.2) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.8.2  (abort with ^G)

    1> ec_timing:time_direct_vs_signature_rbdict().
    Timing rbdict
    Range: 6 - 15070 mics
    Median: 7 mics
    Average: 7 mics
    Timing ec_dict implementation of ec_dictionary
    Testing ec_rbdict
    Range: 6 - 6013 mics
    Median: 7 mics
    Average: 7 mics
    2>
	```

So no difference it time. Well the reality is that there is a
difference in timing, there must be, but we don't have enough
resolution in the timing system to be able to figure out what that
difference is. Essentially that means it's really, really small - or small
enough not to worry about at the very least.

Conclusion
----------

Signatures are a viable, useful approach to the problem of interfaces
in Erlang. They have little or no overhead depending on the type of
implementation, and greatly increase the flexibility of the a library
while retaining testability and locality.

### Terminology

Behaviour
: A normal Erlang Behaviour that defines a contract

Signature
: A combination of an Behaviour and functionality to make the
  functions callable in a concrete way

Native Signature Implementation
: A module that implements a signature directly

Signature Wrapper
: A module that does translation between a preexisting module and a
  Signature, allowing the preexisting module to be used as a Signature
  Implementation.

### Code Referenced

* [ec_dictionary Implementation](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_dictionary.erl)
* [ec_dict Signature Wrapper](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_dict.erl)
* [ec_rbdict Native Signature Implementation](https://github.com/ericbmerritt/erlware_commons/blob/types/src/ec_rbdict.erl)
* [ec_timing Signature Use Example and Timing Collector](https://github.com/ericbmerritt/erlware_commons/blob/types/examples/ec_timing.erl)
