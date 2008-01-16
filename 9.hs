main = putStrLn . show . head $ [ a*b*c | a <- [1..999], b <- [1..a], let c = 1000-a-b, c^2==a^2+b^2 ]
