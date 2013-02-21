sortLength x y 
      | length x > length y = GT
      | length x < length y = LT
      | True = EQ -- there must be a better way to do that!

--sortBy sortLength [[2,3],[1]]
