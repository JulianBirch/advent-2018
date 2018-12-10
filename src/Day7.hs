{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Day7 where

import qualified Text.Megaparsec as MP
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(foldl')
import Data.Maybe(fromMaybe)
import Debug.Trace(traceShow)
import qualified Utility as U
import qualified Parsing as P
import qualified Data.Set as S

import Control.Lens.Operators
import Control.Monad.State.Strict(MonadState, StateT, get, evalStateT)
import Control.Monad.Reader(MonadReader)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Combinators(many)

import Control.Lens(Lens', lens, makeLenses, view, over, use)
import qualified Control.Lens as Le 
import Utility((<$$>))

-- from -> to
inDegree :: (Ord a) => F.Fold (a, a) (M.Map a Int)
inDegree = F.Fold r M.empty id where
    r m (f, t) = M.insertWith (flip const) f 0 $ M.insertWith (+) t 1 m

zeroNodes :: (Ord a) => (M.Map a Int) -> [a]
zeroNodes = map fst . filter ((== 0) . snd) . M.toList

backTrace :: (Ord a, Ord b) => F.Fold (a, b) (M.Map a (S.Set b))
backTrace = F.Fold r M.empty id where
    r m (x, y) = M.insertWith S.union x (S.singleton y) m

initalState :: (Ord a) => F.Fold (a, a) (M.Map a Int, M.Map a (S.Set a))
initalState = (,) <$> inDegree <*> backTrace 

data HahnState a = HahnState {
    _inDeg :: M.Map a Int,
    _zeroN :: S.Set a
} deriving (Show)

makeLenses ''HahnState

nextNode :: forall a . (Show a, Ord a) => (M.Map a (S.Set a)) -> StateT (HahnState a) Maybe a
nextNode bt = do
    xxx <- get
    x <- traceShow xxx $ (lift . S.lookupMin) =<< (use zeroN)
    zeroN %= (S.delete x)
    -- This next bit of nastiness courtesy of the fact Set isn't an instance of Functor
    let nodes = maybe [] S.toList $ M.lookup x bt
    x <$ traverse countDown nodes 
    where countDown :: a -> StateT (HahnState a) Maybe ()
          countDown n = (inDeg . U.atD n 0) <%= (subtract 1) >>= (updateZeroNodes n)
          updateZeroNodes n 0 = zeroN %= (S.insert n)
          updateZeroNodes _ _ = pure ()

hahnAlgorithm :: (Show a, Foldable t, Ord a) => t (a, a) -> [a]
hahnAlgorithm edges = fromMaybe [] xxx where
    (inD, bt) = F.fold ((,) <$> inDegree <*> backTrace) edges
    initialZeroNodes = foldMap S.singleton (zeroNodes inD)
    start = HahnState inD initialZeroNodes 
    xxx = traceShow bt $ evalStateT (many $ nextNode bt) start

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

day7a = hahnAlgorithm <$$> day7Input
day7atest = hahnAlgorithm <$$> day7Test