main = putStrLn . show . sum . evenFibs $ 1000000

evenFibs n = filter even . takeWhile (<=n) $ fib
fib = 1:2:zipWith (+) fib (tail fib)
