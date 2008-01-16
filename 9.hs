main = putStrLn . show . $ [ (a,b,c) | a <- [1..999], b <- [1..a], let c = isqrt (a^2+b^2), a+b+c==1000, c^2==a^2+b^2 ]
isqrt = floor . sqrt . fromIntegral
