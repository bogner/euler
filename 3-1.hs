main = putStrLn . show . last . take 1000 $ primes
primes = 2:(sieve [3,5..])
    where sieve (x:xs) = x:(sieve . filter ((/=0) . flip mod x) $ xs)
          sieve [] = []
factors 0 = []
factors n = first:(factors . div n $ first)
    where first = head . filter ((==0) . mod n) $ primes

primes'' = 2 : [ n | n <- [3,5..], prime n ]
prime n = all ((/= 0) . (mod n)) $ takeWhile (<=isqrt n) primes
isqrt = floor . sqrt . fromIntegral

primes' = 2:(sieve [3,5..])
    where sieve (x:xs) = x:(sieve [ y | y <- xs, mod y x /= 0 ])

splitWhen _ []          =  ([],[])
splitWhen p (x:xs) 
            | p x       =  (x : l, r)
            where (l,r) = splitWhen p xs
            | otherwise =  xs
y <- 
