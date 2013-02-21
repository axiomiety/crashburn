--[1,2,3,2,1], [1,2,3,3,2,1]

isPalindrome [x] = True
isPalindrome [x,y] = x == y
isPalindrome (x:xs) = let rev = reverse xs
                      in x == (head rev) && (isPalindrome (tail rev))
