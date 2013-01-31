lastButOne (x:y:[]) = x
lastButOne (x:y:xs) = lastButOne (y:xs)
