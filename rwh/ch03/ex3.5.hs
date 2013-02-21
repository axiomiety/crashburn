--Write a function that computes the mean of a list, i.e. the sum of all elements in the list divided by its length. (You may need to use the fromIntegral function to convert the length of the list from an integer into a floating point number.)

myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

mySum [x] = x
mySum (x:xs) = x + mySum xs

myMean :: [Int] -> Float
myMean [x] = fromIntegral x
myMean x = let len = myLength x 
               s   = mySum x
           in fromIntegral s/ fromIntegral len
