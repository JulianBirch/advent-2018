{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, DeriveFoldable #-}
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
import Control.Monad.State.Strict(StateT(..), execStateT, runStateT)
import Control.Applicative(many)
import Data.Proxy(Proxy(..))
import Data.Maybe(fromMaybe, fromJust) -- blech
import Data.Coerce(coerce, Coercible)
import Data.Function(on)
import Control.Monad(guard)

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

class StackLike zn where
    type Elem zn
    push :: Elem zn -> zn -> zn
    pop :: zn -> Maybe (Elem zn, zn) 

data LexicalPriority a = LexicalPriority (S.Set a) 
    deriving (Show, Foldable)

constructLP :: (Foldable t, Ord a) => t a -> LexicalPriority a 
constructLP = LexicalPriority . foldr S.insert S.empty

instance (Ord a) => StackLike (LexicalPriority a) where
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
liftPop :: forall s zn a . (StackLike zn, a ~ Elem zn) => Lens' s zn -> StateT s Maybe a 
liftPop lens = StateT $ mas where
    mazn :: s -> Maybe (a, zn)
    mazn s = pop (s ^. lens)
    mas :: s -> Maybe (a, s)
    mas s = (Le._Just . Le._2) %~ (flip (lens .~) s) $ mazn s

nextNode :: forall zn a. (Ord a, StackLike zn, a ~ Elem zn) => 
        (M.Map a [a]) -> StateT (HahnState zn) Maybe a
nextNode backTrace = do
    result <- liftPop zeroN
    let nodes = fromMaybe [] $ M.lookup result backTrace
    result <$ traverse countDown nodes 
    where countDown :: (Elem zn) -> StateT (HahnState zn) Maybe ()
          countDown n = (inDeg . U.atD n 0) <%= (subtract 1) >>= (updateStackLike n)
          updateStackLike n 0 = zeroN %= (push n)
          updateStackLike _ _ = pure ()

hahnAlgorithm :: forall zn t a . (StackLike zn, Foldable t, Ord a, a ~ Elem zn) 
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

-- Can't figure out a good way to parameterize the cost function
data BStrategy t a = BStrategy {
    _current :: t,
    _working :: LexicalPriority (t, a),
    _waiting :: LexicalPriority a
} deriving (Show) 

makeLenses 'BStrategy

newtype BTestStrategy = BTest (BStrategy Int Char) deriving (Show)

constructBS :: [Char] -> BStrategy Int Char
constructBS initial = BStrategy {
    _current = 0,
    _working = constructLP [],
    _waiting = constructLP initial
}

type BStateT a t v = StateT (BStrategy t a) Maybe v

assignWorkers :: forall a t . (Ord a, Ord t, Num t) => (a -> t) -> Int -> BStateT a t ()
assignWorkers cost maxWorkers = () <$ (many assignWorkerLimited)
    where assignWorkerLimited = (<$ (guard =<< workersAvailable)) =<< assignWorkerReal
          workersAvailable = (<=) <$> (length <$> (use working)) <*> pure maxWorkers
          assignWorkerReal = do
                next <- liftPop waiting
                c <- use current
                working %= push (c + cost next, next)
        
class AssignmentStrategy b where
    assign :: Proxy b -> BStateT Char Int ()

instance AssignmentStrategy (BStrategy Int Char) where
    assign = const $ assignWorkers bCost 5
        where bCost z = 61 + on (-) fromEnum z 'A'

instance AssignmentStrategy BTestStrategy where
    assign = const $ assignWorkers bCostTest 2
        where bCostTest z = 1 + on (-) fromEnum z 'A'

pushB :: forall b . (Coercible (BStrategy Int Char) b, AssignmentStrategy b) => Char -> b -> b
pushB x = coerce . fromJust . (execStateT (assign (Proxy :: Proxy b))) . (waiting %~ push x) . coerce

popB :: forall b . (Coercible (BStrategy Int Char) b, AssignmentStrategy b) => b -> Maybe (Char, b)
popB = coerce <$> runStateT $ (assign (Proxy :: Proxy b) *> bPop) 
    where bPop = do
            (c, result) <- liftPop working
            current .= c
            pure result

-- Not sure I know a good way to avoid this duplication
instance StackLike (BStrategy Int Char) where
    type Elem (BStrategy Int Char) = Char
    push = pushB
    pop = popB

instance StackLike (BTestStrategy) where
    type Elem BTestStrategy = Char
    push = pushB
    pop = popB
    
day7b :: P.ParseResult IO Int 
day7b = (view current . snd . hahnAlgorithm constructBS) <$$> day7Input

day7btest :: P.ParseResult IO BTestStrategy
day7btest = (snd . hahnAlgorithm (BTest . constructBS)) <$$> day7Test