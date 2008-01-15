main = putStrLn . show . foldr max 0 . getPals $ [100..999]
coms []     = []
coms (x:xs) = map ((,) x) (x:xs) ++ coms xs
getPals x = filter isPal . uncurry (zipWith (*)) . unzip . coms $ x
--getPals x = filter (isPal . uncurry (*)) . coms $ x
isPal n = (==n) . read . reverse . show $ n
