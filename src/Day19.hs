{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Lens.Operators
import Data.Maybe(fromJust)
import Control.Applicative(Alternative)
import Data.Char(toLower)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import qualified Parsing as P
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Control.Lens(use, makeLenses, view, ix, preview, preuse)
import Control.Monad.State.Strict(StateT, MonadState, execStateT)
import Control.Monad.Reader(ReaderT, MonadReader, runReaderT)

import Utility((<$$>))
import qualified Utility as U

import Day16

data Day19File = Day19File {
    _instructionCounterRegister :: Int,
    _instructions :: IM.IntMap (Instruction OpCode)
} deriving (Show)

makeLenses ''Day19File

enumParser :: (Enum a, Bounded a) => (a -> String) -> P.Parser a
enumParser f = (m M.!) <$> MP.many MPC.letterChar
    where m = foldMap (M.singleton <$> f <*> id) U.allEnum

fileParser :: P.Parser Day19File
fileParser = do
    ic <- MP.chunk "#ip " *> MPCL.decimal <* MPC.newline
    is <- P.lineParser (instructionParser $ enumParser (toLower <$$> show))
    pure $ Day19File ic (IM.fromList $ zip [0..] is)

day19Input :: P.ParseResult IO Day19File
day19Input = P.parseAdventFile fileParser 19

data Day19State = Day19State {
    _reg :: [Int],
    _ic :: Int
} deriving (Show)

makeLenses ''Day19State

instance HasRegisters (Day19State) where
    registers = reg

extract :: (Alternative m, Monad m) => m (Maybe a) -> m a
extract = (U.liftAlternative =<<)

exec19 :: (MonadReader Day19File m, MonadState Day19State m, Alternative m) => m ()
exec19 = do
    localIc <- use ic
    icr <- view instructionCounterRegister
    (register icr) .= localIc
    instruction <- (extract . preview) $ instructions . ix localIc 
    exec16 instruction
    newIc <- extract . preuse $ register icr
    ic .= newIc + 1

-- day19 :: (ReaderT Day19File (StateT Day19State Maybe) () -> ReaderT Day19File (StateT Day19State Maybe) a) -> Int -> P.ParseResult IO Day19State
day19 :: (MonadReader Day19File m, MonadState Day19State m, Alternative m) =>
        (m () -> StateT Day19State (ReaderT Day19File Maybe) a)
        -> Int
        -> P.ParseResult IO Day19State
day19 raise n = (fromJust .) (runReaderT $ execStateT (raise exec19) (Day19State [n,0,0,0,0,0] 0)) <$$> day19Input 

day19a :: P.ParseResult IO Int
day19a = fromJust . preview (reg . ix 1) <$$> day19 MP.many 0

-- By examining the code, we can figure out register 1 has the target "n" in it, and we just need to sum the divisors
day19b :: P.ParseResult IO Int
day19b = sum . (\n -> filter (\z -> 0 == n `mod` z) [1..n]) . fromJust . preview (reg . ix 1) <$$> day19 (MP.count 30) 1