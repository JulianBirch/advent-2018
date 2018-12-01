
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day1 where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S

import Data.Foldable(foldlM)
import Data.Void(Void)

type Parser a = forall m . MP.ParsecT Void String m a -- No custom errors, String parser, using IO

intParser :: Parser Int
intParser = g <$> s <*> n where
    s = MP.oneOf "+-"
    n = L.decimal
    g '-'= (0-)
    g _ = id

lineParser :: Parser a -> Parser [a]
lineParser p = MP.many (p <* C.char '\n')

fileParser :: Parser [Int]
fileParser = lineParser intParser

parseFromFile p file = MP.runParser p file <$> readFile file

day1Input = parseFromFile (lineParser intParser) "C:\\Users\\me\\advent2018\\day1.txt"

day1a = (fmap . fmap) sum day1Input

firstRepeat :: forall f a . (Foldable f, Ord a) => f a -> Maybe a
firstRepeat = leftToMaybe . foldlM f S.empty
    where f :: (S.Set a) -> a -> Either a (S.Set a)
          f s x | S.member x s = Left x
                | otherwise = Right (S.insert x s) 
          leftToMaybe :: (Either a b) -> Maybe a
          leftToMaybe (Left a) = Just a
          leftToMaybe _ = Nothing
                

day1b = (fmap . fmap) (firstRepeat . scanl (+) 0 . cycle) day1Input
