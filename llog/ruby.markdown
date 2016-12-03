---
layout: default
title: llog - ruby
category: pages
---

#### E

The syntax is funky. What do you mean everything is an object?

    irb(main):001:0> 3.class
    => Fixnum

Ah.

#### E

So to define a *class* method, I need to preface the name with `self.`?

    class Foo
      def self.bar

Coming from Python, that's very unintutive.

#### E

I'm not sure I get the fuss about `do` and `end`. It's a little annoying to remember to close blocks/methods/classes. I guess that's the kind of thing a decent IDE would help with.
Saying that I'm getting the hang of code blocks - and they rock!

#### E

There's no built-in unittest framework it seems. I had to get something called `minitest`, but it wasn't readily compatible with the tests on [exercism.io](http://www.exercism.io). I had to change `Minitest::Test` to `MiniTest::Unit::TestCase`.

#### E

So strings aren't lists of chars. `'foo'.respond_to?('each')` is false, unlike `['f','o','o'].respond_to?('each')`.

But help's around the corner:

    irb(main):009:0> 'fOo'.each_char { |c| puts c.swapcase }
    F
    o
    O
    => "fOo"

Sounds like half the battle is knowing what's available.

#### E

The support for hashes is really neat. And iteration is guaranteed to be in the order inserted (as of 1.9 onwards I think). And you have to love the `invert` method - that's so hacky!

#### E

Concatenation in place using `#<<` - that's neat and it works for strings.

#### E

Symbols - funky little things. I particularly like the way you get to use them in hashes.

#### E

Inclusive ranges! Via `(1..10)` or exclusive like `(1...10)`. And `find`/`find_all` - more fun with code blocks

    irb(main):017:0> (1..10).find_all { |i| i%2 == 0}
    => [2, 4, 6, 8, 10]

#### E

Turning a range into an array:

    irb(main):018:0> (1..10).class
    => Range
    irb(main):019:0> [*1..10].class
    => Array

We can also remove in-place:

    irb(main):025:0> a.delete_if{|i| i % 2 == 0}
    => [1, 3, 5, 7, 9]

#### E

What's this `merge` on hashes? It take a *code block* to handle conflicts? Now *that* is cool.

    >> h1 = {:toast => 'nutella', :oats => 'honey'}
    => {:toast=>"nutella", :oats=>"honey"}
    >> h2 = {:fruit => 'apple', :oats => 'jam'}
    => {:fruit=>"apple", :oats=>"jam"}
    >> h1.merge(h2) { |k,from_h1, from_h2| [from_h1, from_h2] } 
    => {:toast=>"nutella", :oats=>["honey", "jam"], :fruit=>"apple"}

If I had any doubts now, they're gone! Though note that this returns a *new* hash. To merge `h1` in-place, we need to call `merge!`.

#### E

`collect` and `map` - number of items in = number of items out. They're not funky list comprehensions.

#### E

The spaceship operator: `<=>`, which is short-hand for `cmp` in other languages. 

    >> a=[8,1,4,3,7]
    => [8, 1, 4, 3, 7]
    >> a.sort {|a1, a2| a1 <=> a2}
    => [1, 3, 4, 7, 8]
    >> a
    => [8, 1, 4, 3, 7]
    >> a.sort! {|a1, a2| a1 <=> a2}
    => [1, 3, 4, 7, 8]
    >> a
    => [1, 3, 4, 7, 8]

There's also a `sort_by` method - which helps us define an item's property to sort by. And note that for hashes, Ruby first converts the hash into an array (so the values that get passed down are themselves `(k,v)` arrays (if only there were tuples!).

#### E

No kw arguments? Back to using the hash workaround just like with perl5. That's a shame... Still have defaults though.

#### E

No tuples and you can only return a single object. Thankfully Ruby can do multiple assignment in-place - so `x,y = get_point`, assuming `get_point` returns an array with 2 values. We can even leave out the square brackets, which *almost* makes it look like we tuples!

#### E

Looks like there isn't unpacking by default. The below is a little ugly - I would have much preferred for Ruby to pass in `|memo,s1,s2|`

    seq1.each_char.zip(seq2.each_char).inject(0) { |memo, s| s[0] == s[1] ? memo : memo+1 }

#### E

Instance variables can't be accessed directly. At all. Ever. Instead access needs to be done via getters setters. Not sure how I feel about that, but `attr_reader`,`attr_writer` and `attr_accessor` do make it easier (and they take symbols!). *However* those don't work with *class* variables (though it seems Ruby On Rails has a custom `cattr` method to do just that).

#### E

`self` can represent both the instance or the class. I guess using `cls` in Python is more convention than anything else.

#### E

We can have macros! Via `define_method` - so we can call methods (from mixins, say) which in turn add new methods to the class.

#### E

There's a `module` keyword - just like `class`. But you cannot create objects from modules or sublcass them. Useful as namespaces among other things.

#### E

When you set a class variable in a sub class, you set it for the superclass and all other subclasses too.

#### E

You can't use `yield` inside `define_method` (see [here](http://stackoverflow.com/questions/2306731/using-yield-inside-define-method-in-ruby)).
