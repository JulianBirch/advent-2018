{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures -Wno-unused-matches #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Day7 where

import qualified Text.Megaparsec as MP
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Utility as U
import qualified Parsing as P

import Control.Lens.Operators
import Control.Lens(Lens', lens, makeLenses, view, over, use)
import Control.Monad.State.Strict(MonadState, StateT(..), get, execStateT, evalStateT, runStateT, put)
import Control.Monad.Reader(MonadReader)
import Control.Monad.Trans.Class(lift)
import Control.Applicative(empty, (<|>), many, Alternative)
import Data.Proxy(Proxy(..))
import Data.List(foldl')
import Data.Maybe(listToMaybe, fromMaybe, maybe, fromJust) -- blech
import Data.Coerce(coerce)
import Debug.Trace(traceShow)
import Safe.Foldable(maximumMay, minimumMay, minimumByMay)
import Safe(lastMay)
import Data.Function(on)
import Control.Monad(guard, MonadPlus)

import qualified Control.Lens as Le 
import qualified Control.Lens.Traversal as LT 
import qualified Control.Lens.Type as LTy 
import Utility((<$$>))

-- from -> to
inDegree :: (Ord a) => F.Fold (a, a) (M.Map a Int)
inDegree = F.Fold r M.empty id where
    r m (f, t) = M.insertWith (flip const) f 0 $ M.insertWith (+) t 1 m

zeroNodes :: (Ord a) => (M.Map a Int) -> [a]
zeroNodes = map fst . filter ((== 0) . snd) . M.toList

backTrace :: (Ord a, Ord b) => F.Fold (a, b) (M.Map a [b])
backTrace = F.Fold r M.empty (fmap S.toList) where
    r m (x, y) = M.insertWith S.union x (S.singleton y) m

type PopType x a = x -> Maybe (a, x)

class ZeroNodes zn where
    type Elem zn
    push :: Elem zn -> zn -> zn
    pop :: PopType zn (Elem zn) 

data LexicalPriority a = LexicalPriority (S.Set a) 
    deriving (Show)

constructLP :: (Foldable t, Ord a) => t a -> LexicalPriority a 
constructLP = LexicalPriority . foldr S.insert S.empty

instance (Ord a) => ZeroNodes (LexicalPriority a) where
    type Elem (LexicalPriority a) = a
    push x (LexicalPriority s) = LexicalPriority $ S.insert x s
    pop (LexicalPriority s) = ((,) <$> id <*> f) <$> x 
        where x = S.lookupMin s
              f x = LexicalPriority $ S.delete x s


data HahnState zn = HahnState {
    _inDeg :: M.Map (Elem zn) Int,
    _zeroN :: zn
}

deriving instance (Show zn, Show (Elem zn)) => Show (HahnState zn)

makeLenses ''HahnState

-- It would be nice to turn liftPopType into a lens, but I don't think it's possible
liftPopType :: forall s a x . Lens' s a -> (PopType a x) -> (PopType s x) 
liftPopType lens popA s = mxs where
    mxa :: Maybe (x, a)
    mxa = popA (s ^. lens)
    mxs :: Maybe (x, s)
    mxs = (Le._Just . Le._2) %~ (flip (lens .~) s) $ mxa

nextNodeRaw :: (Ord (Elem zn), ZeroNodes zn) => StateT (HahnState zn) Maybe (Elem zn)
nextNodeRaw = StateT $ liftPopType zeroN pop

nextNode :: forall zn . (Show zn, Show (Elem zn), Ord (Elem zn), ZeroNodes zn) => 
        (M.Map (Elem zn) [(Elem zn)]) -> StateT (HahnState zn) Maybe (Elem zn)
nextNode backTrace = do
    x <- nextNodeRaw
    let nodes = fromMaybe [] $ M.lookup x backTrace
    x <$ traverse countDown nodes 
    where countDown :: (Elem zn) -> StateT (HahnState zn) Maybe ()
          countDown n = (inDeg . U.atD n 0) <%= (subtract 1) >>= (updateZeroNodes n)
          updateZeroNodes n 0 = zeroN %= (push n)
          updateZeroNodes _ _ = pure ()

-- Achievement unlocked: figured out why people write "a ~ Elem zn"
hahnAlgorithm :: forall zn t a . (Show zn, Show a, ZeroNodes zn, Foldable t, Ord a, a ~ Elem zn) 
    => ([a] -> zn) -> t (a, a) -> ([a], zn)
hahnAlgorithm construct edges = over Le._2 (view zeroN) $ fromMaybe ([], start) xxx  where
    (inD, bt) = F.fold ((,) <$> inDegree <*> backTrace) edges
    start = HahnState inD (construct $ zeroNodes inD) 
    xxx = runStateT (many $ nextNode bt) start

edgeParser :: P.Parser (Char, Char)
edgeParser = (,) <$> x <*> y where
    x :: P.Parser Char
    x = (MP.chunk "Step ") *> MP.anySingle
    y :: P.Parser Char
    y = (MP.chunk " must be finished before step ") *> MP.anySingle <* (MP.chunk " can begin.")

day7Input :: P.ParseResult IO [(Char, Char)]
day7Input = P.parseAdventFile (P.lineParser edgeParser) 7

day7Test :: P.ParseResult IO [(Char, Char)]
day7Test = P.parseAdventFile' (P.lineParser edgeParser) (P.adventFile' "7test")

day7a = hahnAlgorithm constructLP <$$> day7Input
day7atest = hahnAlgorithm constructLP <$$> day7Test

data BStrategy a = BStrategy {
    _current :: Int,
    _maxWorkers :: Int,
    _working :: [(Int, a)],
    _waiting :: LexicalPriority a
} deriving (Show) 

makeLenses 'BStrategy

bCost z = 61 + on (-) fromEnum z 'A'
bCostTest z = 1 + on (-) fromEnum z 'A'

constructBS :: [Char] -> BStrategy Char
constructBS initial = BStrategy {
    _current = 0,
    _maxWorkers = 5,
    _working = [],
    _waiting = constructLP initial
}

type BStateT a v = StateT (BStrategy a) Maybe v

joinMaybe :: (Monad m, Alternative m) => m (Maybe a) -> m a
joinMaybe = ((maybe empty pure) =<<)

lastAlternative :: (Monad m, Alternative m) => m a -> m a
lastAlternative = joinMaybe . fmap lastMay . many

try :: (Alternative m, MonadPlus m) => a -> StateT s m a -> StateT s m a 
try v s = do
    originalState <- get
    s <|> (put originalState *> pure v)

assignWorkerReal :: forall a. (Ord a) => (a -> Int) -> BStateT a ()
assignWorkerReal cost = do
    next <- StateT $ liftPopType waiting pop
    c <- use current
    working %= (:) (c + cost next, next)

workersAvailable :: BStateT a Bool
workersAvailable = (<=) <$> (length <$> (use working)) <*> (use maxWorkers)  

assignWorker' :: forall a. (Ord a) => (a -> Int) -> BStateT a ()
assignWorker' cost = (<$ (guard =<< workersAvailable)) =<< (assignWorkerReal cost)

assignWorkers :: forall a. (Ord a, Show a, Show (BStrategy a)) => (a -> Int) -> BStateT a ()
assignWorkers cost = do
    let x = try [] (many $ assignWorker' cost)
    xxx <- get
    traceShow ("XX" ++ (show xxx)) $ (() <$ x)

nextTime :: BStateT a Int
nextTime = lift =<< nextOrCurrent
    where next = minimumMay <$> fst <$$> (use working)
          nextOrCurrent = next <|> (Just <$> (use current))

bPop :: forall a. (Show a, Ord a) => (a -> Int) -> BStateT a a
bPop cost = do
    xxx <- get
    traceShow xxx $ assignWorkers cost
    c <- nextTime
    current .= c
    (_, result) <- lift =<< minimumByMay (on compare fst) . filter ((== c) . fst) <$> (use working)
    working %= filter ((/= result) . snd)
    assignWorkers cost 
    xyz <- get
    traceShow (result, xyz) $ pure result

instance ZeroNodes (BStrategy Char) where
    type Elem (BStrategy Char) = Char
    push x = (waiting %~ push x) 
    pop = runStateT $ bPop bCost

newtype BTestStrategy a = BTest (BStrategy a) deriving (Show)

instance ZeroNodes (BTestStrategy Char) where
    type Elem (BTestStrategy Char) = Char
    push x = coerce . (waiting %~ push x) . coerce
    pop = coerce <$> runStateT $ bPop bCostTest

day7b = hahnAlgorithm constructBS <$$> day7Input
day7btest = hahnAlgorithm (BTest . (maxWorkers .~ 2). constructBS) <$$> day7Test

initialTestState = BStrategy {_current = 0, _maxWorkers = 2, _working = [], _waiting = constructLP "AF"}