main = putStrLn . show . sum . takeWhile (<1000000) $ primes

primes = 2 : filter prime [3,5..]
prime n = all ((/= 0) . mod n) $ takeWhile (<=isqrt n) primes
isqrt = floor . sqrt . fromIntegral
