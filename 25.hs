main = putStrLn . show $ p24 1000

p24 n = head $ dropWhile ((<10^999) . snd) (zip [1..] fibs)

fibs = 1:1:zipWith (+) fibs (tail fibs)
