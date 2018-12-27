{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day8 where

import Data.Functor.Foldable(cata, Fix(..))
import Data.Function(fix)
import Data.Functor.Compose(Compose(..))
import Utility((<$$>))
import Text.Show.Deriving(deriveShow1)
import Safe(atMay)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as Le
import qualified Text.Megaparsec.Char as C
import qualified Parsing as P 

data Node s = Node {
    subNodes :: [s],
    metaData :: [Integer]
} deriving (Functor, Show)

deriveShow1 ''Node

nodeParser :: P.Parser a -> P.Parser (Node a)
nodeParser component = do
    childNodeCount <- Le.decimal <* C.space
    metaNodeCount <- Le.decimal <* C.space
    childNodes <- MP.count childNodeCount (component <* C.space)
    metaData <- P.countSepBy metaNodeCount Le.decimal C.space
    pure $ Node childNodes metaData 

day8Input :: P.ParseResult IO (Fix Node)
day8Input = P.parseAdventFile parser 8
    where parser = (fix $ fmap Fix . nodeParser) <* C.newline

day8a :: P.ParseResult IO Integer
day8a = cata f <$$> day8Input where
    f n = sum (metaData n) + sum (subNodes n)

day8bReduce :: Node Integer -> Integer
day8bReduce n | 0 == length (subNodes n) = sum (metaData n)
              | otherwise = sum . Compose $ lookup . fromIntegral . (subtract 1) <$> (metaData n)
                    where lookup = atMay (subNodes n)
                          
day8b :: P.ParseResult IO Integer
day8b = cata day8bReduce <$$> day8Input