main = putStrLn . show . last . factors $ 317584931803

{-primes = sieve (2:[3,5..])
    where sieve (x:xs) = x:(sieve . filter ((/=) 0 . flip mod x) $ xs)
          sieve [] = []

filterWhile _ _ []          =  []
filterWhile p f (x:xs) | p x       = (x:xs)
                       | f x       = x:(filterWhile p f xs)
                       | otherwise = filterWhile p f xs
-}

primes = 2 : filter prime [3,5..]
prime n = all ((/= 0) . mod n) $ takeWhile (<=isqrt n) primes
isqrt = floor . sqrt . fromIntegral

factors 1 = []
factors n = first:(factors . div n $ first)
    where first = head . filter ((==0) . mod n) $ primes
