-- file: ch03/letwhere.hs
bar = let b = 2
          c = True
      in let a = b
         in (a, c)
