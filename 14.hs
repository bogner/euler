main = putStrLn . show $ p14 1000000

p14 n = maxIndex $ collatz' (div n 2) n

maxIndex :: (Num a, Ord a) => [(a, a)] -> a
maxIndex = fst . foldl maxSnd (0,0)
    where maxSnd x@(_,x') y@(_,y') = if x' <= y' then y else x

collatz :: Integral a => a -> a -> [(a,a)]
collatz min max =
    let collatz n | 0 == n    = 0
                  | 1 == n    = 1
                  | even n    = (1+) . collatz $ div n 2
                  | otherwise = (1+) . collatz $ 3 * n + 1
    in zip [min..] $ map collatz [min..max]

collatz' :: Integral a => a -> a -> [(a,a)]
collatz' min max =
    let collatz n | 0 == n    = 0
                  | 1 == n    = 1
                  | even n    = (1+) . collatz $ div n 2
                  | otherwise = (1+) . collatz $ 3 * n + 1
    in zip [min',min'+2..] $ map collatz [min',min'+2..max]
        where min' = (div min 2) * 2 + 1
