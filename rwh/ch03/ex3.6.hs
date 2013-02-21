pal (x:xs) = let rev = reverse (x:xs)
             in x : xs ++ rev
