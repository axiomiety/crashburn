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
#### E
#### E
#### E
#### E
#### E
