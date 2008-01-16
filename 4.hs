main = putStrLn . show . maximum $ palindromes 100 999
    where palindromes l h = [ x * y | x <- [l..h], y <- [x..h], p (x * y) ]
          p n = s == reverse s where s = show n
