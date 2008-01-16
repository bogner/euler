main = putStrLn . show . f 2 $ 317584931803
     where f p 1 = p
           f p n | r == 0    = f p q
                 | otherwise = f (p + 1) n
               where (q,r) = divMod n p
