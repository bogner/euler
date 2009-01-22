main = putStrLn . show $ p206

p206 = head . filter (check . (^2)) $ possibles

possibles = concatMap (\x -> x:x+40:[]) [10^9+30,10^9+130..]

check :: Int -> Bool
check = maybe False (const True) . check' [0,9,8,7,6,5,4,3,2,1]

check' :: [Int] -> Int -> Maybe ()
check' [] _     = Just ()
check' (x:xs) n = next x n >>= check' xs

next :: Int -> Int -> Maybe Int
next x y | mod y 10 == x = Just $ div y 100
         | otherwise     = Nothing
