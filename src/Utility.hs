{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Utility((<$$>), (<$$$>), leftToMaybe, groupByKey, atD, atD2, allEnum,
    liftAlternative, AtWithDefault(..)) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Foldable(foldl')
import Control.Lens(Lens', lens, Index, IxValue)
import Control.Applicative(Alternative, empty)

leftToMaybe :: (Either a b) -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe _ = Nothing

infixr 8 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixr 8 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

groupByKey :: forall a b t . (Ord b, Foldable t) => (a -> b) -> t a -> M.Map b [a]
groupByKey f l = foldl' (flip r) M.empty l where
    g :: a -> M.Map b [a]
    g a = M.singleton (f a) [a]
    r :: a -> M.Map b [a] -> M.Map b [a]
    r a m = M.unionWith (++) m (g a)

atD :: (Ord k) => k -> v -> Lens' (M.Map k v) v
atD k d = lens get set where
    get m = M.findWithDefault d k m 
    set m v = M.insert k v m

atD2 :: (AtWithDefault m1, AtWithDefault m2, m2 ~ IxValue m1) => (IxValue m2) -> (Index m1) -> (Index m2) -> Lens' m1 (IxValue m2)
atD2 v k1 k2 = (atWithD atEmpty k1) . (atWithD v k2)

class AtWithDefault m where
    atEmpty :: m
    atWithD :: IxValue m -> Index m -> Lens' m (IxValue m)

instance (Ord k) => AtWithDefault (M.Map k v) where
    atEmpty = M.empty
    atWithD d k = lens get set where
        get m = M.findWithDefault d k m 
        set m v = M.insert k v m

instance AtWithDefault (IM.IntMap v) where
    atEmpty = IM.empty
    atWithD d k = lens get set where
        get m = IM.findWithDefault d k m 
        set m v = IM.insert k v m

allEnum :: (Bounded a, Enum a) => [a]
allEnum = enumFrom minBound

-- Like !! but decent diagnostics when it fails
(!.!) :: (Show a) => [a] -> Int -> a
list !.! n | n < length list = list !! n
           | otherwise = error ("Failed to lookup " ++ (show n) ++ " in " ++ (show list))

liftAlternative :: (Alternative m) => Maybe a -> m a
liftAlternative = maybe empty pure

           
