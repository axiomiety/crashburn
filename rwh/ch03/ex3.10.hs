data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

td1 = Node "x" Empty Empty
td2 = Node "x" Empty (Node "y" Empty Empty)


depth Empty = 0
depth (Node a b c) = 1 + max (depth b) (depth c)
