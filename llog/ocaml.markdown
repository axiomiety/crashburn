strings: "yo"
chars: 'a'

Lists
[1;2;3]

Arrays use the `|` delemiter:
[|"Foo";"Bar"|]


# List.rev [1;2;3;];;
- : int list = [3; 2; 1]

# List.hd[1;2;3];;
- : int = 1

# Array.append [|1;2|][|3;4|];;
- : int array = [|1; 2; 3; 4|]

Arrays are 0-indexed

# Array.get[|1;2;3;4|] 0;;
- : int = 1
# [|1;2;3;4|].(-1);;
Exception: Invalid_argument "index out of bounds".
# [|1;2;3;4|].(3);;
- : int = 4

Can't use -1

# String.make 3 'o';;
- : string = "ooo"
# "foo" ^ "bar";;
- : string = "foobar"
# String.length "foobar";;
- : int = 6
# Array.length;;
- : 'a array -> int = <fun>
# Array.length "foobar";;
File "", line 1, characters 13-21:
Error: This expression has type string but an expression was expected of type
         'a array
		 
So arrays and strings are different types!

# String.lowercase "FOOBAR";;
- : string = "foobar"
# String.concat '/' ["path";"to";"nowhere"];;
File "", line 1, characters 14-17:
Error: This expression has type char but an expression was expected of type
         string
# String.concat "/" ["path";"to";"nowhere"];;
- : string = "path/to/nowhere"
# String.concat;;
- : string -> string list -> string = <fun>

Best to look at the fn signature to know what to give it!

# int_of_string "foo";;
Exception: Failure "int_of_string".
# int_of_string "123";;
- : int = 123

Tuples are special

# (32,'a',"abc", false);;
- : int * char * string * bool = (32, 'a', "abc", false)
# fst (32,'a',"abc", false);;
File "", line 1, characters 4-25:
Error: This expression has type 'a * 'b * 'c * 'd
       but an expression was expected of type 'e * 'f
# fst (32,'a');;
- : int = 32
# snd (32,'a');;
- : char = 'a'

Can only use `fst` and `snd` with pairs


Use `let` to assign vars

# let x = 3;;
val x : int = 3
# let x = x+1;;
val x : int = 4

Not sure what that left arrow operator is

# x <- x+1;;
File "", line 1, characters 0-8:
Error: The value x is not an instance variable

There's a <type> ref type.

# let x = ref 3;;
val x : int ref = {contents = 3}

This is an `int ref` type.

# x := 100/4;;
- : unit = ()
# x;;
- : int ref = {contents = 25}

Type system into action!

# let y = x+1;;
File "", line 1, characters 8-9:
Error: This expression has type int ref
       but an expression was expected of type int

Need to use `!` to 'unref' the reference
	   
# let y = !x+1;;
val y : int = 26

Printing looks a bit odd. Built-ins don't mangle types

# Printf.printf "x = %d, bye %s\n" !x "John";;
- : unit = ()
# print_string 32;;
File "", line 1, characters 13-15:
Error: This expression has type int but an expression was expected of type
         string
# print_string "32";;
- : unit = <unknown constructor>
# print_int 3;;
- : unit = <unknown constructor>
# ;
File "", line 1, characters 13-14:
Error: Syntax error
# print_int 2;;
- : unit = <unknown constructor>
# print_newline ();;
- : unit = ()
# print_int 2;;

And I think the online console doesn't render properly.

Lists?

# let x1 = ref [];;
val x1 : '_a list ref = {contents = []}
# x1 := 0 :: !x1;;
- : unit = ()
# x1;;
- : int list ref = {contents = [0]}
# x1 := 1 :: !x1;;
- : unit = ()
# x1;;
- : int list ref = {contents = [1; 0]}
# x1 := 2 :: !x1;;
- : unit = ()
# x1;;
- : int list ref = {contents = [2; 1; 0]}
# List.rev !x1;;
- : int list = [0; 1; 2]
# x1;;
- : int list ref = {contents = [2; 1; 0]}

Can't seem to do for loops on the prompt. Format is:

for i = 1 to 10 do
  xl := i :: !xl;
done;

for i = 10 downto 1 do
 xl := i :: !xl
done;

booleans


# 1>2;;
- : bool = false
# 1 = 1;;
- : bool = true
# 1 <> 1;;
- : bool = false
# "1" = 1;;
File "", line 1, characters 6-7:
Error: This expression has type int but an expression was expected of type
         string
# "1" = string_of_int 1;;
- : bool = true
# "aa" > "ab";;
- : bool = false
# "aaa" > "ab";;
- : bool = false
# "ab" > "aa";;
- : bool = true

