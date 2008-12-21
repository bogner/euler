import Data.List (sort)
import Data.Maybe (fromJust)

input = "22.in"

main = readFile input >>= putStrLn . show . p22

p22 = sum . zipWith eval [1..] . sort . names

eval pos name = pos * (sum $ map letterVal name)

letterVal = fromJust . flip lookup (zip ['A'..'Z'] [1..])

names = split ',' . filter (/='"')

split _ []  = []
split c str = s:split c (drop 1 ss)
    where (s,ss) = break (==c) str
