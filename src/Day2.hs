{-# LANGUAGE TypeApplications #-}

module Day2 where

import qualified Parsing as P
import Data.Foldable(foldl')
import Data.List(sort, group, elem)
import Control.Applicative(liftA2)
import Control.Monad(guard)
import Data.Bool(bool)
import Data.Maybe(listToMaybe, catMaybes)
import qualified Data.Map.Strict as M

import Utility ((<$$>))

day2Text :: IO [String]
day2Text = lines <$> (readFile $ P.adventFile 2)

xxx = "oeambtcgjqnzhgkdylfapoiusr"

freq :: (Ord a, Eq a) => [a] -> [Int]
freq =  fmap length . group . sort

countN :: (Eq a) => a -> [[a]] -> Int
countN n = length . filter (elem n) 

-- day1a = (countN 2 <$> (fmap.fmap) freq day2Text) * (countN 3 <$> (fmap.fmap) freq day2Text)

missing :: Int -> [a] -> [a]
missing n l = (take n l) ++ (drop (n+1) l)

foldWithMerge :: (Foldable t, Semigroup m, Ord k) => t (M.Map k m) -> M.Map k m 
foldWithMerge = M.unionsWith (<>) 

groupBy :: (Ord b, Foldable t, Functor t) => (a -> b) -> t a -> M.Map b [a]
groupBy f l = foldWithMerge (g <$> l) where
    g a = M.singleton (f a) [a]

findPair :: (Foldable t) => b -> t a -> Maybe b
findPair k vs  = k <$ guard (2 == (length vs)) 

testInput = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]

day2b = catMaybes <$> (M.foldMapWithKey findPair) <$$> (traverse (groupBy . missing) [0..25]) <$> day2Text
