---
layout: default
title: llog/buildyourownlisp
category: pages
---

Notes from my journey going through [buildyourownlisp](http://buildyourownlisp.com) with no prior C experience (hence some of the n00b comments).

#### E

add -ggdb to get debug symbols. this will allow gdb to, among other things, display the source code. seems you can also use -g, but that's more generic. source: https://gcc.gnu.org/onlinedocs/gcc-3.4.5/gcc/Debugging-Options.html

#### E

`puts` vs `printf` on [stackoverflow]( http://stackoverflow.com/questions/2454474/what-is-the-difference-between-printf-and-puts-in-c)

nice answer from 'Hannu Balk' that looks at the assembly equivalent, and when the compiler tries to be clever

TODO: understand what puts actually does, and its return value!

#### E

main has `argc` (arg *c*ount) and `argv` (arg *v*ector).

_"in a definition, int main() is a function taking no parameters. In a declaration which is not a definition, it's a function taking unspecified parameters. This is 6.7.5.3/14 in n1256, or 6.5.5.3/10 in n794. The questioner is asking about a definition. – Steve Jessop Oct 9 '10 at 23:19 "_

whaaaat?

As per the [C standard](http://c-faq.com/ansi/maindecl.html), there should only be 2 valid declarations.

#### E

To convert a numberic arg passed to `main`, use `atoi` from `stdlib`. If you don't pass one and you're directly referencing `argv[1]`, it will segfault (core dumped)!. Need to see if that can be investigated.

#### E

The core dump won't necessarily reside in the same directory. To see where, check out `/proc/sys/kernel/core_pattern`. If it mentions `abrt` it's something that manages those dumps, the config for which resides in `/etc/abrt/abrt-action-save-package-data.conf`.

More info on [abrt](https://github.com/abrt/abrt/wiki/ABRT-Project), which stands for Another Bug Report Tool. Who knew...

#### E

To analyse the dump:

    cd /var/spool/abrt/XXXXXXX
    gdb $(cat executable) coredump

This directory contains a whole bunch of files (including one named executable!)

Note that `abrt` can sometimes lock down the dumps. Might need to be root.

#### E

C99, it seems, added a new `_Bool` type via `<stdbool.h>` - with `true` and `false`!

#### E

`atoi` returns 0 if it cannot find a number at the start of the string.

#### E

When defining structs, it can be useful to do `typedef struct StructName { ... } TypeDefName;`. This means that instead of defining StructName vars, you don't need to use `struct StructName x;` - but instead `TyepDefName x;`.

#### E

Tried the `bool` type - all good! Seems we're not really to use `_Bool` directly unless you're maintaining code that has macros for `bool`,`true`, or `false`.

#### E

I was wondering where the header files (like `editline/readline.h`) lived. It turns out that if you pass the `-H` switch to `cc` it will show the full path of every include file ("The dots at the beginning of each line count how deeply nested the #include is."). Note that the `#include` statements are processed by the preprocessor. Most of those will live under `/usr/include`.

#### E

Tried to include `editline` headers in my program, failed big time. After some searching around I ended up installing `readline` (`yum install readline-devel`). However that wasn't enough, and I had to add the `lreadline` flag for the compiler o_O.

The `-lreadline` flag tells the complier to link to the `readline` library.

#### E

To write cross-platform C, you often need to use the preprocessor. On windows, `#ifdef _WIN32` is usually used by compilers, and `#ifdef __unix__` otherwise. Other platforms like FreeBSD might define `__FreeBSD__`. See [this page](http://nadeausoftware.com/articles/2012/01/c_c_tip_how_use_compiler_predefined_macros_detect_operating_system) for a comprehensive set of macros.
