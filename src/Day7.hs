{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Day7 where

import qualified Text.Megaparsec as MP
import qualified Control.Foldl as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(foldl')
import Data.Maybe(fromMaybe, maybe)
import Debug.Trace(traceShow)
import qualified Utility as U
import qualified Parsing as P
import qualified Data.Set as S

import Control.Lens.Operators
import Control.Monad.State.Strict(MonadState, StateT, get, evalStateT)
import Control.Monad.Reader(MonadReader)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Combinators(many)
import Data.Proxy(Proxy(..))

import Control.Lens(Lens', lens, makeLenses, view, over, use)
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

initalState :: (Ord a) => F.Fold (a, a) (M.Map a Int, M.Map a [a])
initalState = (,) <$> inDegree <*> backTrace 

class ZeroNodes zn where
    type Elem zn
    construct :: [Elem zn] -> zn
    push :: Elem zn -> zn -> zn
    pop :: zn -> (zn, Maybe (Elem zn))

data LexicalPriority a = LexicalPriority (S.Set a) 
    deriving (Show)

instance (Ord a) => ZeroNodes (LexicalPriority a) where
    type Elem (LexicalPriority a) = a
    construct = LexicalPriority . foldr S.insert S.empty
    push x (LexicalPriority s) = LexicalPriority $ S.insert x s
    pop (LexicalPriority s) = (ns, x)
        where x = S.lookupMin s
              ns = LexicalPriority $ fromMaybe S.empty $ S.delete <$> x <*> pure s

data HahnState zn = HahnState {
    _inDeg :: M.Map (Elem zn) Int,
    _zeroN :: zn
}

deriving instance (Show zn, Show (Elem zn)) => Show (HahnState zn)

makeLenses ''HahnState

nextNode :: forall zn . (Show zn, Show (Elem zn), Ord (Elem zn), ZeroNodes zn) => (M.Map (Elem zn) [(Elem zn)]) -> StateT (HahnState zn) Maybe (Elem zn)
nextNode bt = do
    xxx <- get
    (ns, maybeX) <- traceShow xxx $ pop <$> (use zeroN)
    zeroN .= ns
    (x :: Elem zn) <- lift maybeX
    let nodes = fromMaybe [] $ M.lookup x bt
    x <$ traverse countDown nodes 
    where countDown :: (Elem zn) -> StateT (HahnState zn) Maybe ()
          countDown n = (inDeg . U.atD n 0) <%= (subtract 1) >>= (updateZeroNodes n)
          updateZeroNodes n 0 = zeroN %= (push n)
          updateZeroNodes _ _ = pure ()

hahnAlgorithm :: forall zn t . (Show zn, Show (Elem zn), ZeroNodes zn, Foldable t, Show (Elem zn), Ord (Elem zn)) => Proxy zn -> t (Elem zn, Elem zn) -> [Elem zn]
hahnAlgorithm _ edges = fromMaybe [] xxx where
    (inD, bt) = F.fold ((,) <$> inDegree <*> backTrace) edges
    start = HahnState inD (construct $ zeroNodes inD :: zn) 
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

day7a = hahnAlgorithm (Proxy :: Proxy (LexicalPriority Char)) <$$> day7Input
day7atest = hahnAlgorithm (Proxy :: Proxy (LexicalPriority Char)) <$$> day7Test
{--}