
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day1 where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import qualified Parsing as P
import qualified Utility as U

import Utility ((<$$>))
import Data.Foldable(foldlM)

intParser :: P.Parser Int
intParser = g <$> MP.oneOf "+-" <*> L.decimal where
    g '-'= (0-)
    g _ = id

day1Input :: P.ParseResult IO [Int]
day1Input = P.parseAdventFile (P.lineParser intParser) 1

day1a :: P.ParseResult IO Int
day1a = sum <$$> day1Input

firstRepeat :: forall f a . (Foldable f, Ord a) => f a -> Maybe a
firstRepeat = U.leftToMaybe . foldlM f S.empty
    where f :: (S.Set a) -> a -> Either a (S.Set a)
          f s x | S.member x s = Left x
                | otherwise = Right (S.insert x s) 
                
day1b :: P.ParseResult IO (Maybe Int)
day1b = (firstRepeat . scanl (+) 0 . cycle) <$$> day1Input
