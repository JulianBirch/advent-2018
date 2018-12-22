{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day8 where

import Data.Functor.Foldable(cata, Fix(..))
import Data.Maybe(fromMaybe)
import Utility((<$$>))
import Text.Show.Deriving(deriveShow, deriveShow1)
import Safe(atMay)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as Le
import qualified Text.Megaparsec.Char as C
import qualified Parsing as P 

data Node s = Node {
    subNodes :: [s],
    metaData :: [Integer]
} deriving (Functor)

deriveShow ''Node
deriveShow1 ''Node

space :: P.Parser Char
space = MP.single ' '

nodeParser :: P.Parser Char -> P.Parser (Fix Node)
nodeParser terminator = Fix <$> do  -- I have no idea why it doesn't work if I just put (Fix . Node) inside the do
    childNodeCount <- Le.decimal <* space
    metaNodeCount <- Le.decimal <* space
    childNodes <- MP.count childNodeCount (nodeParser space)
    metaData <- P.countSepBy metaNodeCount Le.decimal space
    _ <- terminator
    pure $ Node childNodes metaData 

day8Input :: P.ParseResult IO (Fix Node)
day8Input = P.parseAdventFile (nodeParser C.newline) 8

day8a :: P.ParseResult IO Integer
day8a = cata f <$$> day8Input where
    f n = sum (metaData n) + sum (subNodes n)

day8bReduce :: Node Integer -> Integer
day8bReduce n | 0 == length (subNodes n) = sum (metaData n)
              | otherwise = sum $ lookup . fromIntegral . (subtract 1) <$> (metaData n)
                    where lookup = (fromMaybe 0) . (atMay (subNodes n))
                          
day8b :: P.ParseResult IO Integer
day8b = cata day8bReduce <$$> day8Input


