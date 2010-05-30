import Control.Applicative

main = putStrLn . show $ p36

p36 = sum . filter palindromicInBothBases $ [1,3..onemillion]
    where onemillion = 1000000

palindromicInBothBases = liftA2 (&&) palindromicInBase2 palindromicInBase10
    where palindromicInBase2  = palindromic . binaryDigits
          palindromicInBase10 = palindromic . decimalDigits

palindromic items = take half items == take half (reverse items)
    where half = length items `div` 2

decimalDigits = digitsInBase 10
binaryDigits  = digitsInBase 2
digitsInBase b x = reverse . collect $ calculate
    where collect = map snd . takeWhile (/=(0,0))
          calculate = iterate (getNext . fst) (getNext x)
          getNext = flip divMod b
