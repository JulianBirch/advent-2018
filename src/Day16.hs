{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches -Wno-dodgy-imports -Wno-type-defaults #-} 
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16 where

import Debug.Trace(traceShow)
import Control.Monad.State.Strict(State(..), execState, runStateT)
import Control.Lens.Operators
import Control.Lens(preuse, reuse, use, selfIndex, makeLenses, at, ix)
import Data.Maybe(fromMaybe)
import Data.Bool(bool)
import Data.Bits((.&.), (.|.))
import Utility((<$$>), atD)
import Data.Function(on)
import Data.List(unfoldr)
import Control.Monad(guard)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import qualified Parsing as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Machine = State [Int]

data Input = Register | Value

data OpCode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
    | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Bounded, Enum, Show)

getValue :: Input -> Int -> Machine Int
getValue Value n = pure n
getValue Register n = fromMaybe 0 <$> preuse (ix n)

setValue :: Int -> Int -> Machine ()
setValue pos val = ix pos .= val

to10 :: Bool -> Int
to10 = bool 0 1

interpretOpCode :: OpCode -> (Input, Input, Int -> Int -> Int)
interpretOpCode Addr = (Register, Register, (+)) 
interpretOpCode Addi = (Register, Value, (+))
interpretOpCode Mulr = (Register, Register, (*)) 
interpretOpCode Muli = (Register, Value, (*))
interpretOpCode Banr = (Register, Register, (.&.)) 
interpretOpCode Bani = (Register, Value, (.&.))
interpretOpCode Borr = (Register, Register, (.|.)) 
interpretOpCode Bori = (Register, Value, (.|.))
interpretOpCode Setr = (Register, Value, const) 
interpretOpCode Seti = (Value, Value, const)
-- I don't much like the code duplication here. Not sure how to address it though
interpretOpCode Gtir = (Value, Register, (to10 .) . (>)) 
interpretOpCode Gtri = (Register, Value, (to10 .) . (>))
interpretOpCode Gtrr = (Register, Register, (to10 .) . (>)) 
interpretOpCode Eqir = (Value, Register, (to10 .) . (==)) 
interpretOpCode Eqri = (Register, Value, (to10 .) . (==))
interpretOpCode Eqrr = (Register, Register, (to10 .) . (==)) 
     
exec :: Instruction OpCode -> Machine ()
exec (Instruction (opCode,a,b,c)) = do
    let (ia, ib, op) = interpretOpCode opCode
    x <- op <$> (getValue ia a) <*> (getValue ib b)
    setValue c x

newtype Instruction a = Instruction (a,Int,Int,Int)
    deriving (Functor, Show)

data Scenario = Scenario {
    before :: [Int],
    instruction :: Instruction Int,
    after :: [Int]
} deriving (Show)

repack :: [a] -> (a,a,a,a)
repack (a:b:c:d:_) = (a,b,c,d)
repack _ = error "Repack failed"

instructionParser :: P.Parser (Instruction Int)
instructionParser = MP.label "Instruction" $ (Instruction . repack) <$> P.countSepBy 4 MPCL.decimal MPC.space

parseMachineState = MP.between (MP.single '[') (MP.single ']') (P.countSepBy 4 MPCL.decimal (MP.chunk ", "))

scenarioParser :: P.Parser Scenario
scenarioParser = do
    b <- MP.label "Before" $ MP.chunk "Before: " *> parseMachineState
    _ <- MPC.newline
    i <- instructionParser
    _ <- MPC.newline
    a <- MP.label "After" $ MP.chunk "After:  " *> parseMachineState
    _ <- MP.count 2 MPC.newline
    pure $ Scenario b i a
    where 

data Day16File = Day16File {
    scenarios :: [Scenario],
    instructions :: [Instruction Int]
} deriving (Show)

day16FileParser :: P.Parser Day16File
day16FileParser = do
    s <- MP.many scenarioParser
    _ <- MP.many MPC.newline
    i <- P.lineParser instructionParser
    pure $ Day16File s i

day16Input = P.parseAdventFile day16FileParser 16

verifyScenario :: Scenario -> OpCode -> Bool
verifyScenario s opCode = on (==) ($ s) after (execState state . before)  where
    state = exec $ (const opCode) <$> instruction s

allEnum :: (Bounded a, Enum a) => [a]
allEnum = enumFrom minBound

day16a = (length . filter ((>= 3) . length . getOpCodes) . scenarios) <$$> day16Input

testScenario = MP.runParser scenarioParser "" "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n\n"

getOpCodes s = S.fromList $ filter (verifyScenario s) allEnum


{-}
possibleOpCodes :: Scenario -> [OpCode]
possibleOpCodes = 
-}