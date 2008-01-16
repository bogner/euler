main = putStrLn . show $ [ (a,b,c) | a <- squares, b <- squares, c2 <- (+) a b, isSquare c2, c <- isqrt c2, a+b+c==1000]
squares = map (^2) [0..1000]
isSquare :: Int -> Bool
isSquare = flip elem squares
--sumSquares x y = filter (flip elem squares) $ zipWith (+) x y
isqrt = floor . sqrt . fromIntegral
