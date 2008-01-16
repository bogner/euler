main = putStrLn . show $ diffSums 10000000000000000000000000000000000000000000000000
--diffSums n = (div (n*(n+1)) 2)^2 - (div (n*(n+1)*(2*n+1)) 6)
diffSums n = div (3*n^4+2*n^3-3*n^2-2*n) 12
