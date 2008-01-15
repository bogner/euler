main = putStrLn . show $ [ c | a <- squares, b <- squares, c2 <- zipWith (+) a b, elem c squares, a+b+c==1000]
squares = map (^2) . [0..1000]
