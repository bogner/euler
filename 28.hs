main = putStrLn . show $ p28

p28 :: Int
p28 = sum $ corners 1001

corners :: Int -> [Int]
corners n = 1 : corners' 1 (takeWhile (<n) addList)
    where corners' _ [] = []
          corners' x (y:ys) = z : corners' z ys
              where z = x + y

addList :: [Int]
addList = concatMap (take 4 . repeat) [2,4..]
