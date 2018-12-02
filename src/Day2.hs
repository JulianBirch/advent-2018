{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day2 where

import Data.List(elem)
import Control.Monad(guard)
import Data.Bool(bool)
import Data.Maybe(catMaybes)
import qualified Data.Map.Strict as M

import qualified Parsing as P
import qualified Utility as U
import Utility ((<$$>))

day2Text :: IO [String]
day2Text = lines <$> (readFile $ P.adventFile 2)

countN :: (Ord a) => Int -> [[a]] -> Int
countN n = sum . fmap ((bool 0 1 <$> elem n) . fmap length . M.elems . U.groupByKey id) 

testInputA :: [String]
testInputA = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

checksum :: Ord a => [[a]] -> Int
checksum input = product $ countN <$> [2,3] <*> pure input

day1a :: IO Int
day1a = checksum <$> day2Text

missing :: Int -> [a] -> [a]
missing n l = (take n l) ++ (drop (n+1) l)

findPair :: (Foldable t) => b -> t a -> Maybe b
findPair k vs  = k <$ guard (2 == (length vs)) 

testInputB :: [String]
testInputB = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]

day2b :: IO String
day2b = (head . catMaybes) <$> (M.foldMapWithKey findPair) <$$> (traverse (U.groupByKey . missing) [0..25]) <$> day2Text
