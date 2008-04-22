main = getLine >>= putStrLn . show . sum . map length . toN . fromIntegral . read

toNine   = [ "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine" ]
prefices = [ "Thir", "Four", "Fif", "Six", "Seven", "Eigh", "Nine" ]
teens    = ["Ten", "Eleven", "Twelve"] ++ map (++"teen") prefices
tens     = ["Twenty","Thirty","Forty"]++(map (++"ty") . drop 2 $ prefices)
bigger   = ["Hundred","Thousand","Million","Billion"]
bigSplit = "And"
rest     = map (\x -> map (++x) toNine) bigger

toN = flip take $ fillUp bigSplit toHundred rest

fillNames :: [Char] -> [[Char]] -> [[Char]] -> [[Char]]
fillNames split ns ps = ns++concatMap (\x -> x:map ((x++split)++) ns) ps

fillUp :: [Char] -> [[Char]] -> [[[Char]]] -> [[Char]]
fillUp split ns [] = ns
fillUp split ns (p:ps) = fillUp split (fillNames split ns p) ps

toHundred  = start++teens++rest
    where
      (start,rest) = splitAt 9 $ fillNames "" toNine tens
