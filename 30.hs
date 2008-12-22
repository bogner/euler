main = putStrLn . show $ p30

p30 = sum . writtenAsPowers $ 5

writtenAsPowers :: Int -> [Int]
writtenAsPowers n = filter (\x -> x == nthPowers n x) [2..big]
    where big = (10^) . head $ dropWhile possible [1..]
          possible x = 10^x < nthPowers n (10^x-1)

nthPowers :: Int -> Int -> Int
nthPowers n = sum . map ((^n) . read . (:[])) . show
