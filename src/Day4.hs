{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Day4 where

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Parsing as P
import qualified Utility as U

import Utility ((<$$>))
import Data.Foldable(foldlM, foldl')
import Control.Applicative((<|>))
import Data.List(sortBy, maximumBy)
import Data.Function(on)
import Data.Maybe(fromMaybe)
import Control.Lens.Operators
import Control.Monad.State.Strict(State, get, evalState)
import Control.Monad.Reader(MonadReader)
import Debug.Trace(traceShowId)

import Control.Lens -- (makeLenses)

data Date = Date {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
} deriving (Ord, Eq, Show)

dateParser :: P.Parser Date
dateParser = do
        y <- L.decimal
        m <- x '-'
        d <- x '-'
        h <- x ' '
        mi <- x ':'
        pure (Date { year = y, month = m, day = d, hour = h, minute = mi}) 
    where x c = MP.single c *> L.decimal

newtype GuardId = GuardId Int deriving (Show, Ord, Eq)

data Event = WakeUp | FallAsleep | BeginShift GuardId 
    deriving (Show)

eventParser :: P.Parser Event
eventParser = wu <|> fa <|> bs where
    f :: Event -> String -> P.Parser Event
    f e l = e <$ (MP.try $ MP.chunk l)
    wu :: P.Parser Event 
    wu = f WakeUp " wakes up" 
    fa :: P.Parser Event 
    fa = f FallAsleep " falls asleep" 
    bs :: P.Parser Event 
    bs = MP.chunk " Guard #" *> ((BeginShift . GuardId) <$> L.decimal) <* MP.chunk " begins shift"

parse :: String -> P.ParseResult IO [(Date, Event)]
parse x = sort <$$> P.parseAdventFile'' (P.lineParser parser) x where
    parser = (,) <$> (MP.single '[' *> dateParser <* MP.single ']') <*> eventParser
    sort = sortBy (on compare fst)

day4Input :: P.ParseResult IO [(Date, Event)]
day4Input = parse (P.adventFile 4)

day4TestInput :: P.ParseResult IO [(Date, Event)]
day4TestInput = parse (P.adventFile' "4test")

atD :: (Ord k) => k -> v -> Lens' (M.Map k v) v
atD k d = lens get set where
    get m = M.findWithDefault d k m 
    set m v = M.insert k v m

atD2 :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Lens' (M.Map k1 (M.Map k2 v)) v
atD2 k1 k2 v = (atD k1 M.empty) . (atD k2 v)
    
type GuardMinuteCount = M.Map GuardId (M.Map Int Int)
    -- Guard, Minute, Count
    -- Asleep Data     
data GuardState = GuardState {
    _asleepSince :: Maybe Int,
    _currentGuard :: GuardId
}  deriving (Show)

makeLenses ''GuardState

initialGuardState :: GuardState
initialGuardState = GuardState {
    _asleepSince = Nothing,
    _currentGuard = GuardId 0
}

update :: (MonadReader GuardState m) => GuardMinuteCount -> Int -> m (GuardMinuteCount)
update gmc to = do
    since <- fromMaybe 0 <$> (view asleepSince) -- default will never be used
    cg <- view currentGuard
    let increment i = over (atD2 cg i 0) (+1)
    let range = [since..(to-1)]
    pure $ foldl' (flip increment) gmc range

processEvent :: (Date,Event) -> GuardMinuteCount -> State GuardState GuardMinuteCount
processEvent (_, (BeginShift id)) gmc = do
    currentGuard .= id
    asleepSince .= Nothing
    pure gmc
processEvent (d, FallAsleep) gmc = do
    asleepSince .= Just (minute d)
    pure gmc
processEvent (d, WakeUp) gmc = do
    xxx <- get
    let from = update gmc (minute d) xxx
    asleepSince .= Nothing
    pure from

process :: (Foldable t) => t (Date, Event) -> State GuardState GuardMinuteCount
process = foldlM (flip processEvent) M.empty

process2 :: (Foldable t) => t (Date, Event) -> GuardMinuteCount
process2 = flip (evalState . process) initialGuardState

pickBest :: (Ord v2) => (v -> v2) -> M.Map k v -> (k, v)
pickBest f x = maximumBy (on compare (f . snd)) (M.toList x)

bestGuardAndMinute :: GuardMinuteCount -> (GuardId, Int)
bestGuardAndMinute gmc = (fst . pickBest id) <$> (pickBest sum gmc)

bestMinuteAndGuard = pickBest snd . (fmap (pickBest id))

day4a = (bestGuardAndMinute . process2) <$$> day4Input

day4b = (bestMinuteAndGuard . process2) <$$> day4Input