main = putStrLn . show $ p24 1000

p24 n = head $ dropWhile ((<n) . digits . snd) (zip [1..] fibs)

digits = length . show

fibs = 1:1:zipWith (+) fibs (tail fibs)
