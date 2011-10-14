Property based testing for unit testers
=======================================

Main contributors: Torben Hoffmann, Raghav Karol, Eric Merritt

The purpose of the short document is to help people who are familiar
with unit testing understand how property based testing (PBT) differs,
but also where the thinking is the same.

This document focusses on the PBT tool
[`PropEr`](https://github.com/manopapad/proper) for Erlang since that is
what I am familiar with, but the general principles applies to all PBT
tools regardless of which language they are written in.

The approach taken here is that we hear from people who are used to
working with unit testing regarding how they think when designing
their tests and how a concrete test might look.

These descriptions are then "converted" into the way it works with
PBT, with a clear focus on what stays the same and what is different.

## Testing philosophies

### A quote from Martin Logan:

> For me unit testing is about contracts. I think about the same things
> I think about when I write statements like {ok, Resp} =
> Mod:Func(Args). Unit testing and writing specs are very close for me.
> Hypothetically speaking lets say a function should return return {ok,
> string()} | {error, term()} for all given input parameters then my
> unit tests should be able to show that for a representative set of
> input parameters that those contracts are honored. The art comes in
> thinking about what that set is.


The trap in writing all your own tests can often be that we think
about the set in terms of what we coded for and not what may indeed be
asked of our function. As the code is tried in further exploratory
testing and in production new input parameter sets for which the given
function does not meet the stated contract are discovered and added to
the test case once a fix has been put into place.

This is a very good description of what the ground rules for unit
testing are:

* Checking that contracts are obeyed.
* Creating a representative set of input parameters.

The former is very much part of PBT - each property you write will
check a contract, so that thinking is the same.

## xUnit vs PBT

Unit testing has become popular for software testing with the advent
of xUnit tools like jUnit for Java.  xUnit like tools typically
provide a testing framework with the following functionality

*    test fixture setup
*    test case execution
*    test fixture teardown
*    test suite management
*    test status reporting and management

While xUnit tools provide a lot of functionality to execute and manage
test cases and suites, reporting results there is no focus on test
case execution step, while this is the main focus area of
property-based testing (PBT).

Consider the following function specification

    :::erlang
    sort(list::integer()) ---> list::integer() | error

A verbal specification of this function is,

> For all input lists of integers, the sort function returns a sorted
> list of integers.

For any other kind of argument the function returns the atom error.

The specification above may be a requirement of how the function
should behave or even how the function does behave. This distinction
is important; the former is the requirement for the function, the
latter is the actual API. Both should be the same and that is what our
testing should confirm. Test cases for this function might look like

    :::erlang
    assertEqual(sort([5,4,3,2,1]), [1,2,3,4,5])
    assertEqual(sort([1,2,3,4,5]), [1,2,3,4,5])
    assertEqual(sort([]         ), []         )
    assertEqual(sort([-1,0, 1]  ), [-1, 0, 1] )

How many tests cases should we write to be convinced that the actual
behaviour of the function is the same as its specification? Clearly,
it is impossible to write tests cases for all possible input values,
here all lists of integers, the art of testing is finding individual
input values that are representative of a large part of the input
space. We hope that the test cases are exhaustive to cover the
specification. xUnit tools offer no support for this and this is where
PBT and PBT Tools like `PropEr` and `QuickCheck` come in.

PBT introduces testing with a large set of random input values and
verifying that the specification holds for each input value
selected. Functions used to generate input values, generators, are
specified using rules and can be simply composed together to construct
complicated values.  So, a property based test for the function above
may look like:

    :::erlang
    FOREACH({I, J, InputList},  {nat(), nat(), integer_list()},
        SUCHTHAT(I < J andalso J < length(InputList),
        SortedList = sort(InputList)
        length(SortedList) == length(InputList)
        andalso
        lists:get(SortedList, I) =< lists:get(SortedList, J))


The property above works as follows

* Generate a random list of integers `InputList` and two natural numbers
  I, J, such that I < J < size of `InputList`
* Check that size of sorted and input lists is the same.
* Check that element with smaller index I is less than or equal to
  element with larger index J in `SortedList`.

Notice in the property above, we *specify* property. Verification of
the property based on random input values will be done by the property
based tool, therefore we can generated a large number of tests cases
with random input values and have a higher level of confidence that
the function when using unit tests alone.

But it does not stop at generation of input parameters. If you have
more complex tests where you have to generate a series of events and
keep track of some state then your PBT tool will generate random
sequences of events which corresponds to legal sequences of events and
test that your system behaves correctly for all sequences.

So when you have written a property with associated generators you
have in fact created something that can create numerous test cases -
you just have to tell your PBT tool how many test cases you want to
check the property on.

## Shrinking the bar

At this point you might still have the feeling that introducing the
notion of some sort of generators to your unit testing tool of choice
would bring you on par with PBT tools, but wait there is more to
come.

When a PBT tool creates a test case that fails there is real chance
that it has created a long test case or some big input parameters -
trying to debug that is very much like receiving a humongous log from
a system in the field and try to figure out what cause the system to
fail.

Enter shrinking...

When a test case fails the PBT tool will try to shrink the failing
test case down to the essentials by stripping out input elements or
events that does not cause the failure. In most cases this results in
a very short counterexample that clearly states which events and
inputs are required to break a property.

As we go through some concrete examples later the effects of shrinking
will be shown.

Shrinking makes it a lot easier to debug problems and is as key to the
strength of PBT as the generators.

## Converting a unit test

We will now take a look at one possible way of translating a unit
test into a PBT setting.

The example comes from Eric Merritt and is about the `add/2` function in
the `ec_dictionary` instance `ec_gb_trees`.

The add function has the following spec:

    :::erlang
    -spec add(ec_dictionary:key(), ec_dictionary:value(), Object::dictionary()) ->
              dictionary().

and it is supposed to do the obvious: add the key and value pair to
the dictionary and return a new dictionary.

Eric states his basic expectations as follows:

1. I can put arbitrary terms into the dictionary as keys
2. I can put arbitrary terms into the dictionary as values
3. When I put a value in the dictionary by a key, I can retrieve that same value
4. When I put a different value in the dictionary by key it does not change other key value pairs.
5. When I update a value the new value in available by the new key
6. When a value does not exist a not found exception is created

The first two expectations regarding being able to use arbritrary
terms as keys and values is a job for generators.

The latter four are prime candidates for properties and we will create
one for each of them.

### Generators

    :::erlang
    key() -> any().

    value() -> any().


For `PropEr` this approach has the drawback that creation and shrinking
becomes rather time consuming, so it might be better to narrow to
something like this:

    :::erlang
    key() -> union([integer(),atom()]).

    value() -> union([integer(),atom(),binary(),boolean(),string()]).

What is best depends on the situation and intended usage.

Now, being able to generate keys and values is not enough. You also
have to tell `PropEr` how to create a dictionary and in this case we
will use a symbolic generator (detail to be explained later).

    :::erlang
    sym_dict() ->
        ?SIZED(N,sym_dict(N)).

    sym_dict(0) ->
        {'$call',ec_dictionary,new,[ec_gb_trees]};
    sym_dict(N) ->
        ?LAZY(
           frequency([
                      {1, {'$call',ec_dictionary,remove,[key(),sym_dict(N-1)]}},
                      {2, {'$call',ec_dictionary,add,[value(),value(),sym_dict(N-1)]}}
                     ])).


`sym_dict/0` uses the `?SIZED` macro to control the size of the
generated dictionary. `PropEr` will start out with small numbers and
gradually raise it.

`sym_dict/1` is building a dictionary by randomly adding key/value
pairs and removing keys. Eventually the base case is reached which
will create an empty dictionary.

The `?LAZY` macro is used to defer the calculation of the
`sym_dict(N-1)` until they are needed and `frequency/1` is used
to ensure that twice as many adds compared to removes are done. This
should give rather more interesting dictionaries in the long run, if
not one can alter the frequencies accondingly.

But does it really work?

That is a good question and one that should always be asked when
looking at genetors. Fortunately there is a way to see what a
generator produces provided that the generator functions are exported.

Hint: in most cases it will not hurt to throw in a
`-compile(export_all).` in the module used to specify the
properties. And here we actually have a sub-hint: specify the
properties in a separate file to avoid peeking inside the
implementation! Base the test on the published API as this is what the
users of the code will be restricted to.

When the test module has been loaded you can test the generators by
starting up an Erlang shell (this example uses the erlware_commons
code so get yourself a clone to play with):

    :::sh
    $ erl -pz ebin -pz test
    1> proper_gen:pick(ec_dictionary_proper:key()).
    {ok,4}
    2> proper_gen:pick(ec_dictionary_proper:key()).
    {ok,35}
    3> proper_gen:pick(ec_dictionary_proper:key()).
    {ok,-5}
    4> proper_gen:pick(ec_dictionary_proper:key()).
    {ok,48}
    5> proper_gen:pick(ec_dictionary_proper:key()).
    {ok,'\036\207_là´?\nc'}
    6> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,2}
    7> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,-14}
    8> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,-3}
    9> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,27}
    10> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,-8}
    11> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,[472765,17121]}
    12> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,true}
    13> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,<<>>}
    14> proper_gen:pick(ec_dictionary_proper:value()).
    {ok,<<89,69,18,148,32,42,238,101>>}
    15> proper_gen:pick(ec_dictionary_proper:sym_dict()).
    {ok,{'$call',ec_dictionary,add,
            [[114776,1053475],
             'fª\020\227\215',
             {'$call',ec_dictionary,add,
                 ['',true,
                  {'$call',ec_dictionary,add,
                      ['2^Ø¡',
                       [900408,886056],
                       {'$call',ec_dictionary,add,[[48618|...],<<...>>|...]}]}]}]}}
    16> proper_gen:pick(ec_dictionary_proper:sym_dict()).
    {ok,{'$call',ec_dictionary,add,
            [10,'a¯\214\031fõC',
             {'$call',ec_dictionary,add,
                 [false,-1,
                  {'$call',ec_dictionary,remove,
                      ['d·ÉV÷[',
                       {'$call',ec_dictionary,remove,[12,{'$call',...}]}]}]}]}}

That does not look too bad, so we will continue with that for now.


### Properties of `add/2`

The first expectation Eric had about how the dictionary works was that
if a key had been stored it could be retrieved.

One way of expressing this could be with this property:

    :::erlang
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

This property reads that for all dictionaries `get/2` using a key
from a key/value pair just inserted using the `add/3` function
will return that value. If that is not the case the property will
evaluate to false.

Running the property is done using `proper:quickcheck/1`:

    :::sh
    proper:quickcheck(ec_dictionary_proper:prop_get_after_add_returns_correct_value()).
    ....................................................................................................
    OK: Passed 100 test(s).
    true


This was as expected, but at this point we will take a little detour
and introduce a mistake in the `ec_gb_trees` implementation and see
how that works.



