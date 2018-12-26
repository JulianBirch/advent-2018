{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Day16(exec16, Instruction(..), OpCode(..), HasRegisters(..), instructionParser) where

import Control.Monad.State.Strict(MonadState, execState)
import Control.Lens.Operators
import Control.Lens(preuse, ix, Lens')
import Data.Bits((.&.), (.|.))
import Utility((<$$>), allEnum)

import Data.Maybe(fromMaybe)
import Data.Bool(bool)
import Data.Function(on)
import Data.List(unfoldr)
import Control.Monad(guard)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import qualified Parsing as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Input = Register | Value

data OpCode = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori
    | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr 
    deriving (Bounded, Enum, Show, Ord, Eq)

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
     
newtype Instruction a = Instruction (a,Int,Int,Int)
    deriving (Functor, Show)

class HasRegisters s where
  registers :: Lens' s [Int]

exec16 :: forall s m . (MonadState s m) => (HasRegisters s) => Instruction OpCode -> m ()
exec16 (Instruction (opCode,a,b,c)) = do
    let (ia, ib, op) = interpretOpCode opCode
    val <- op <$> (getValue ia a) <*> (getValue ib b)
    (registers . ix c) .= val
    where getValue :: Input -> Int -> m Int
          getValue Value n = pure n
          getValue Register n = fromMaybe (0 :: Int) <$> preuse (registers . ix (fromIntegral n))

data Scenario = Scenario {
    before :: [Int],
    instruction :: Instruction Int,
    after :: [Int]
} deriving (Show)

instructionParser :: P.Parser a -> P.Parser (Instruction a)
instructionParser p = MP.label "Instruction" $ repack <$> p <*> MP.count 3 (MPC.space *> MPCL.decimal)
    where repack a [b,c,d] = Instruction (a,b,c,d)
          repack _ _ = error "Repack failed"

machineStateParser :: P.Parser [Int]
machineStateParser = MP.between (MP.single '[') (MP.single ']') (P.countSepBy 4 MPCL.decimal (MP.chunk ", "))

scenarioParser :: P.Parser Scenario
scenarioParser = do
    b <- MP.label "Before" $ MP.chunk "Before: " *> machineStateParser
    _ <- MPC.newline
    i <- instructionParser MPCL.decimal
    _ <- MPC.newline
    a <- MP.label "After" $ MP.chunk "After:  " *> machineStateParser
    _ <- MP.count 2 MPC.newline
    pure $ Scenario b i a

data Day16File = Day16File {
    scenarios :: [Scenario],
    instructions :: [Instruction Int]
} deriving (Show)

day16FileParser :: P.Parser Day16File
day16FileParser = do
    s <- MP.many scenarioParser
    _ <- MP.many MPC.newline
    i <- P.lineParser $ instructionParser MPCL.decimal
    pure $ Day16File s i

day16Input :: P.ParseResult IO Day16File
day16Input = P.parseAdventFile day16FileParser 16

verifyScenario :: Scenario -> OpCode -> Bool
verifyScenario s opCode = on (==) ($ s) after (execState state . before)  where
    state = exec16 $ (const opCode) <$> instruction s

getOpCodes :: Scenario -> S.Set OpCode
getOpCodes s = S.fromList $ filter (verifyScenario s) allEnum

day16a :: P.ParseResult IO Int
day16a = (length . filter ((>= 3) . length . getOpCodes) . scenarios) <$$> day16Input

-- testScenario = MP.runParser scenarioParser "" "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]\n\n"

only :: S.Set c -> Either c (S.Set c)
only set | 1 == length set = Left $ S.findMin set
         | otherwise = Right $ set

deduceStep :: (Ord a, Ord b) => M.Map a (S.Set b) -> Maybe (M.Map a b, M.Map a (S.Set b))
deduceStep input = (deduced, newInput) <$ guard (not $ M.null deduced) where
    (deduced, newInput') = M.mapEither only input
    valuesToRemove = S.fromList $ M.elems deduced
    newInput = (S.\\ valuesToRemove) <$> newInput'

deduce :: (Ord a, Ord b) => M.Map a (S.Set b) -> M.Map a b
deduce = M.unions . unfoldr deduceStep

opCodeFromScenario :: Scenario -> Int
opCodeFromScenario (Scenario {instruction = (Instruction (i,_,_,_))}) = i

possibilitiesFromFile :: Day16File -> M.Map Int (S.Set OpCode)
possibilitiesFromFile f = M.unions $ (M.singleton <$> opCodeFromScenario <*> getOpCodes) <$> (scenarios f)

instance HasRegisters ([Int]) where
    registers = id

run :: Day16File -> [Int]
run f = execState (traverse exec16 correctInstructions) [0,0,0,0]
    where mapping = (deduce . possibilitiesFromFile) f
          correctInstructions = (mapping M.!) <$$> (instructions f)

day16b :: P.ParseResult IO Int
day16b = (head . run) <$$> day16Input
