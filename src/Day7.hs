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
import Control.Lens(Lens', makeLenses, view, over, use)
import Control.Monad.State.Strict(StateT(..), get, execStateT, runStateT, put)
import Control.Monad.Trans.Class(lift)
import Control.Applicative((<|>), many, Alternative)
import Data.Proxy(Proxy(..))
import Data.Maybe(fromMaybe, fromJust) -- blech
import Data.Coerce(coerce, Coercible)
import Safe.Foldable(minimumMay, minimumByMay)
import Data.Function(on)
import Control.Monad(guard, MonadPlus)

import qualified Control.Lens as Le 
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

class ZeroNodes zn where
    type Elem zn
    push :: Elem zn -> zn -> zn
    pop :: zn -> Maybe (Elem zn, zn) 

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

-- There's got to be better way to do this
-- Achievement unlocked: figured out why people write "a ~ Elem zn"
liftPop :: forall s zn a . (ZeroNodes zn, a ~ Elem zn) => Lens' s zn -> StateT s Maybe a 
liftPop lens = StateT $ mas where
    mazn :: s -> Maybe (a, zn)
    mazn s = pop (s ^. lens)
    mas :: s -> Maybe (a, s)
    mas s = (Le._Just . Le._2) %~ (flip (lens .~) s) $ mazn s

nextNode :: forall zn a. (Ord a, ZeroNodes zn, a ~ Elem zn) => 
        (M.Map a [a]) -> StateT (HahnState zn) Maybe a
nextNode backTrace = do
    result <- liftPop zeroN
    let nodes = fromMaybe [] $ M.lookup result backTrace
    result <$ traverse countDown nodes 
    where countDown :: (Elem zn) -> StateT (HahnState zn) Maybe ()
          countDown n = (inDeg . U.atD n 0) <%= (subtract 1) >>= (updateZeroNodes n)
          updateZeroNodes n 0 = zeroN %= (push n)
          updateZeroNodes _ _ = pure ()

hahnAlgorithm :: forall zn t a . (ZeroNodes zn, Foldable t, Ord a, a ~ Elem zn) 
    => ([a] -> zn) -> t (a, a) -> ([a], zn)
hahnAlgorithm construct edges = over Le._2 (view zeroN) $ fromMaybe ([], start) finalState where
    (inD, bt) = F.fold ((,) <$> inDegree <*> backTrace) edges
    start = HahnState inD (construct $ zeroNodes inD) 
    finalState = runStateT (many $ nextNode bt) start

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

day7a :: P.ParseResult IO String
day7a = (fst . hahnAlgorithm constructLP) <$$> day7Input

day7atest :: P.ParseResult IO String
day7atest = (fst . hahnAlgorithm constructLP) <$$> day7Test

data BStrategy a = BStrategy {
    _current :: Int,
    _working :: [(Int, a)],
    _waiting :: LexicalPriority a
} deriving (Show) 

makeLenses 'BStrategy

newtype BTestStrategy a = BTest (BStrategy a) deriving (Show)

constructBS :: [Char] -> BStrategy Char
constructBS initial = BStrategy {
    _current = 0,
    _working = [],
    _waiting = constructLP initial
}

type BStateT a v = StateT (BStrategy a) Maybe v

try :: (Alternative m, MonadPlus m) => a -> StateT s m a -> StateT s m a 
try v s = do
    originalState <- get
    s <|> (put originalState *> pure v)

assignWorkerReal :: forall a. (Ord a) => (a -> Int) -> BStateT a ()
assignWorkerReal cost = do
    next <- liftPop waiting
    c <- use current
    working %= (:) (c + cost next, next)

assignWorker' :: forall a. (Ord a) => (a -> Int) -> Int -> BStateT a ()
assignWorker' cost maxWorkers = (<$ (guard =<< workersAvailable)) =<< (assignWorkerReal cost)
    where workersAvailable = (<=) <$> (length <$> (use working)) <*> pure maxWorkers  
 
assignWorkers :: forall a. (Ord a) => (a -> Int) -> Int -> BStateT a ()
assignWorkers cost maxWorkers = () <$ try [] (many $ assignWorker' cost maxWorkers)

nextTime :: BStateT a Int
nextTime = lift =<< nextOrCurrent
    where next = minimumMay <$> fst <$$> (use working)
          nextOrCurrent = next <|> (Just <$> (use current))

bPop :: forall a. (Ord a) => BStateT a a
bPop = do
    c <- nextTime
    current .= c
    (_, result) <- lift =<< minimumByMay (on compare fst) . filter ((== c) . fst) <$> (use working)
    working %= filter ((/= result) . snd)
    pure result

class AssignmentStrategy b where
    assign :: Proxy b -> BStateT Char ()

instance AssignmentStrategy (BStrategy Char) where
    assign = const $ assignWorkers bCost 5
        where bCost z = 61 + on (-) fromEnum z 'A'

instance AssignmentStrategy (BTestStrategy Char) where
    assign = const $ assignWorkers bCostTest 2
        where bCostTest z = 1 + on (-) fromEnum z 'A'

pushB :: forall b . (Coercible (BStrategy Char) b, AssignmentStrategy b) => Char -> b -> b
pushB x = coerce . fromJust . (execStateT (assign (Proxy :: Proxy b))) . (waiting %~ push x) . coerce

popB :: forall b . (Coercible (BStrategy Char) b, AssignmentStrategy b) => b -> Maybe (Char, b)
popB = coerce <$> runStateT $ (assign (Proxy :: Proxy b) *> bPop)

instance ZeroNodes (BStrategy Char) where
    type Elem (BStrategy Char) = Char
    push = pushB
    pop = popB

instance ZeroNodes (BTestStrategy Char) where
    type Elem (BTestStrategy Char) = Char
    push = pushB
    pop = popB
    
day7b :: P.ParseResult IO (BStrategy Char)
day7b = (snd . hahnAlgorithm constructBS) <$$> day7Input

day7btest :: P.ParseResult IO (BTestStrategy Char)
day7btest = (snd . hahnAlgorithm (BTest . constructBS)) <$$> day7Test

