{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches -Wno-dodgy-imports #-} 
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Control.Lens.Operators
import Debug.Trace(traceShow)
import Data.Maybe(fromMaybe)
import Data.Bool(bool)
import Data.Function(on)
import Data.List(unfoldr)
import Control.Monad(guard, join)
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
import Control.Monad.State.Strict(StateT, MonadState)
import Control.Monad.Reader(ReaderT, MonadReader)

import Utility((<$$>))
import qualified Utility as U

import Day16

data Day19File = Day19File {
    _instructionCounterRegister :: Int,
    _instructions :: IM.IntMap (Instruction OpCode)
}

makeLenses ''Day19File

enumParser :: (Enum a, Bounded a) => (a -> String) -> P.Parser a
enumParser f = (m M.!) <$> MP.many MPC.letterChar
    where m = foldMap (M.singleton <$> f <*> id) U.allEnum

fileParser :: P.Parser Day19File
fileParser = do
    ic <- MP.chunk "#ip " *> MPCL.decimal <* MPC.newline
    is <- P.lineParser (instructionParser $ enumParser (toLower <$$> show))
    pure $ Day19File ic (IM.fromList $ zip [0..] is)

data Day19State = Day19State {
    _reg :: [Int],
    _ic :: Int
}

makeLenses ''Day19State

instance HasRegisters (Day19State) where
    registers = reg

-- exec19 :: (MonadReader Day19File m, MonadState Day19State m, Monad (m Maybe)) => m Maybe ()
exec19 :: ReaderT Day19File (StateT Day19State Maybe) ()
exec19 = do
    localIc <- use ic
    icr <- view instructionCounterRegister
    (registers . ix icr) .= localIc
    ins <- view instructions
    instruction <- join $ lift <$$$> preview $ instructions . ix localIc
    exec16 instruction
    newIc <- use $ registers . ix icr 
    ic .= newIc + 1
