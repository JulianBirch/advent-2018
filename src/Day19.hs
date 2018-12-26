{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches -Wno-dodgy-imports #-} 
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Day19 where

import Control.Lens.Operators
import Debug.Trace(traceShow)
import Data.Maybe(fromMaybe, fromJust)
import Data.Bool(bool)
import Data.Function(on)
import Data.List(unfoldr)
import Control.Monad(guard, join)
import Control.Applicative(empty, Alternative)
import Control.Monad.Trans.Class(MonadTrans)
import Control.Monad.Fail(MonadFail)
import Data.Char(toLower)
import Safe(atMay)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import qualified Parsing as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import Control.Lens.Operators
import Control.Lens(use, makeLenses, view, ix, preview, preuse)
import Control.Monad.Trans(lift)
import Control.Monad.RWS.Strict(runRWST)
import Control.Monad.State.Strict(StateT, MonadState, execStateT, get)
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

day19Input = P.parseAdventFile fileParser 19

data Day19State = Day19State {
    _reg :: [Int],
    _ic :: Int
} deriving (Show)

makeLenses ''Day19State

instance HasRegisters (Day19State) where
    registers = reg

liftAlternative :: (Alternative m) => Maybe a -> m a
liftAlternative = maybe empty pure

extract :: (Alternative m, Monad m) => m (Maybe a) -> m a
extract = (liftAlternative =<<)

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

exec19 :: (MonadReader Day19File m, MonadState Day19State m, Alternative m) => m ()
-- exec19 :: ReaderT Day19File (StateT Day19State Maybe) ()
exec19 = do
    localIc <- use ic
    icr <- view instructionCounterRegister
    (registers . ix icr) .= localIc
    ins <- view instructions
    instruction <- (extract . preview) $ instructions . ix localIc 
    exec16 instruction
    xxx <- get
    newIc <- extract . preuse $ registers . ix icr
    traceShow xxx $ ic .= newIc + 1

initialState n = Day19State [n,0,0,0,0,0] 0

day19bb c = (Data.Maybe.fromJust .) (runReaderT $ execStateT (MP.count c exec19) (initialState 1)) <$$> day19Input
day19 n = (Data.Maybe.fromJust .) (runReaderT $ execStateT (MP.many exec19) (initialState n)) <$$> day19Input 
day19a = day19 0
day19b = sum $ (\n -> filter (\z -> 0 == n `mod` z) [1..n]) 10551410


{-
day19a = snd3 <$$> flip (runRWST (MP.many exec19)) initialState <$$> day19Input
-}