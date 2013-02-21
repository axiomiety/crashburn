-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 2
         in a + b

quux :: t -> [Char]
quux a = let a = "foo"
         in a ++ "eek!"
