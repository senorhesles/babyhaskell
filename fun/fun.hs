
opposite :: [a] -> [a]
opposite [] = []
opposite (x:xs) = (opposite xs) ++ [x]

factorial n = fac n 1
  where
    fac 0 r = r
    fac n r = fac (n-1) (r*n)





