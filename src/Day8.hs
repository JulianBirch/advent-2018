{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day8 where

import Data.Functor.Foldable(cata, Fix(..))
import Data.Functor.Compose(Compose(..))
import Data.Functor.Classes(Show1)
import Control.Applicative((<|>), Alternative)
import Utility((<$$>))
import Debug.Trace(traceShow)
import Text.Show.Deriving(deriveShow, deriveShow1)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as Le
import qualified Text.Megaparsec.Char as C
import qualified Parsing as P 
-- Taken from https://stackoverflow.com/questions/53549105/exhibiting-the-relationship-between-hylo-and-hylom/53550252#53550252

data Node s = Node {
    subNodes :: [s],
    metaData :: [Integer]
} deriving (Functor)

deriveShow ''Node
deriveShow1 ''Node

space :: P.Parser Char
space = MP.single ' '

countSepBy :: (Alternative m, Monad m) => Int -> m a -> m b -> m [a]
countSepBy n p sep | n > 0 = (:) <$> p <*> MP.count (n-1) (sep *> p) 
countSepBy n p sep | otherwise = pure []

nodeParser :: P.Parser Char -> P.Parser (Fix Node)
nodeParser terminator = Fix <$> do  -- I have no idea why it doesn't work if I just put (Fix . Node) inside the do
    childNodeCount <- Le.decimal <* space
    metaNodeCount <- Le.decimal <* space
    childNodes <- MP.count childNodeCount (nodeParser space)
    metaData <- countSepBy metaNodeCount Le.decimal space
    _ <- terminator
    pure $ Node childNodes metaData 

day8Input :: P.ParseResult IO (Fix Node)
day8Input = P.parseAdventFile (nodeParser C.newline) 8

day8a = cata f <$$> day8Input where
    f n = sum (metaData n) + sum (subNodes n)

day8TestData = MP.parseTest (nodeParser (pure 'x')) "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"