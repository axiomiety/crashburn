---
layout: default
title: llog - erlang
category: pages
---

#### E

Every statement must end with `.`.

#### E

Quitting with `q().` shuts down everything. If connected to a remote VM, do `ctrl+g` followed by `q` and press return.

#### E

You can use `v(<lineno>)` to reference a previous result. Also works with -1, -2 etc... as you'd expect.

#### E

To view the history, type `h()`. You can set the history/result limit via `history(N)` and `results(N)` respectively.

#### E

Funny - `ctrl+t` will transpose the two characters to the left of the cursor - so `abc` will become `acb`.

#### E

Great way to convert numbers - base#value - so `2#101` is 5. _Note: base36 is used by airlines for ticket numbers - like G6ZV1N - though often without 0/o and 1/l_.

#### E

Compile a local file like `drop.erl` with `c(drop)`. Module can now be access via `drop:`.

#### E

In the shell, show current variables with `b()`. You can clear them with `f(<var_name>)` or clear all with `f()`.

#### E

Can bind functions to variables via `FallVelocity = fun(Distance) -> X = (2*9.8*Distance), math::sqrt(X) end.`

#### E

`if` statements can only contain a very few BIF/simple boolean statements. For anything more complex, break it up or use `case`.

#### E

List comprehensions can be used as such: `accumulate(Fn, Ls) -> [Fn(El) || El <- Ls]` - really neat!

#### E

When compiling a function via the shell, you can export all private methods via `c(file_name, [export_all])`. You can even specify that in the file itself via `-compile(pdebug_info, export_all])`. Though it's usually bad practice to leave an `export_all` in there.
