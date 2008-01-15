import List
main = putStrLn . show . sumNotMultsTo $ 999
sumNotMultsTo n = sum $ union [3,6..n] [5,10..n]
