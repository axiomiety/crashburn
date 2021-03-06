---
layout: default
title: llog - erlang
category: pages
---

#### E

When calling a function, `.` precedes the parenthesis - so `foo.(4)` - but if using Erlang's built-in, it's the usual convention (like `:math.sqrt(4)`). Wait. That seems to only be the case when you're defining them in `iex`?

#### E

You can use `v(<lineno>)` to reference a previous result. Also works with -1, -2 etc... as you'd expect.

#### E

`h` is not history but interactive help.

#### E

Unlike Erlang, all functions in a module are exported by default unless they start with `defp` (for def private).

#### E

Defining functions in a module must adhere to the `def <name> do ... end` syntax.

#### E

`@doc` works at the function level and can be accessed via `h <module.fn>`. `@spec` can be accessed with `s <module.fn>`.

#### E

To document module, use `@moduledoc`.

#### E

You can reload & recompile a single module with `r(<module_name>)`.

#### E

Elixir counts from 0!

~~~ erlang
ex(19)> o={:foo,:bar}
{:foo, :bar}
iex(20)> elem(o,1)
:bar
~~~

#### E

Elixir considers anything other than `nil` and `false` to be true.

~~~ erlang
iex(32)> foo=0
0
iex(33)> cond do
...(33)> foo -> true
...(33)> not foo -> false
...(33)> end
true
iex(34)> if [] do true else false end
true
iex(35)> if 0 do true else false end
true
~~~

#### E

 `< ericmj> if, cond, &&, || and ! work with truthy and falsey values`

Guards are not part of this.

#### E

Use `IO.puts` to automatically add a new line (vs `IO.write`). To write out something that isn't a string, use `IO.inspect`:

~~~ erlang
iex(66)> y={1,2,3}
{1, 2, 3}
iex(67)> "y is #{inspect y}"
"y is {1, 2, 3}"
~~~

#### E

Using single quotes will create a list of characters - but are slower to work with and take more space than strings. `'foo' ++ 'bar'`. Convert to and thro with `List.to_string/1` and `String.to_char_list/1`.

#### E

Split a string as a list of words with `~w//`:

~~~ erlang
iex(68)> ~w/The quick brown fox/
["The", "quick", "brown", "fox"]
~~~

#### E

Once a module has been loaded, adding `@doc` and `@moduledoc` won't show up with `r` but `c` instead.

#### E

Arguments to fns should be (start with?) lowercase. Anything starting with a capital letter is a module atom (unlike Erlang)

#### E

`{result, device} = File.open("<filename>", [:read, :utf8])` - `:utf8` is required for text files. To read a single line, `IO.read(<device>, :line)`. Close with `File.close(<device>)`.

#### E

Structs can be updated just like maps:

~~~ erlang
iex(234)> x=%City{name: "Berlin"}
%City{lat: nil, long: nil, name: "Berlin", pop: nil}
iex(235)> %{x | lat: 1.32}
%City{lat: 1.32, long: nil, name: "Berlin", pop: nil}
~~~

#### E

`case` statements that match against variable names need to use pinning - `^`.

#### E

If you're having an issue with an implementation, `mix clean` can sometimes resolve that!

#### E

List comprehensions FTW with tuples:

~~~ erlang
iex(9)> Enum.filter(pp,fn({name, gender, age}) -> (gender == "M") and (age > 40) end)
[{"Tran", "M", 47}, {"Elias", "M", 50}]
~~~

#### E

Actually list comprehensions are this: `for rank <- ["C","D","H","S"], suit <- ["A",2,3,4,5,6,7,8,9,"J","Q","K"], do: {suit, rank}`

#### E

Use `Process.whereis/1` to retrieve the pid of a registered process - and `Process.unregister(<atom>)` to deregister it without killing it. `Process.registered/0` will show all currentl registered processes.

#### E

`Process.flag(:trap_exit, true)` sets you up as receiving a `{:EXIT, pid, reason}` tuple in case of failure.

#### E

Use `spawn_link` to be notified of abnormal exit from the dependent process.

#### E
#### E
#### E
#### E
#### E
#### E
