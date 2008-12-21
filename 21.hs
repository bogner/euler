main = putStrLn . show $ p21 10000

p21 n = sum . filter amicable $ [1..n]

divisors n = filter ((==0) . mod n) [1..n-1]

amicable n = all ($sum . divisors $ n) [(n/=), (==n) . sum . divisors]
