--is :: a -> [[a]] -> [a]

is [x,y] = x ++ "," ++ y
is (x:y:ys) = x ++ "," ++ is (y:ys)

ins separator [] = []
ins separator (x:xs) = x : separator : ins separator xs
