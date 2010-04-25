main = putStrLn . show $ routes 20

routes n = fact(2*n) `div` (fact n)^2

fact n | n > 0     = n * fact (n-1)
       | otherwise = 1
