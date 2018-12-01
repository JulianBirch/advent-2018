module Q1 () where
    
import Data.List (sort)

square :: Integer -> Integer
square x = x * x

sotls :: Integer -> Integer -> Integer -> Integer
sotls x y z = sum . fmap square . drop 1 . sort $ [x,y,z]

sumsq :: Integer -> Integer
sumsq n = sum $ square <$> [1..n]

fact :: Integer -> Integer
fact n = product [1..n]

comb :: Integer -> Integer -> Integer
comb n m | n < m = error "n < m"
         | otherwise = fact n `div` fact m `div` fact (n - m)

mygcd :: Integer -> Integer -> Integer
mygcd n m | m < n = mygcd m n
          | m `mod` n == 0 = n
          | otherwise = mygcd (m `mod` n) n

prime :: Integer -> Bool
prime n = and $ ((/= 0) . (n `mod`)) <$> [2..n-1]

perfect :: Integer -> Bool
perfect n = ((n ==) . sum . (filter $ (== 0) . (n `mod`))) [1..n-1]