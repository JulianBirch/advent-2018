{-# LANGUAGE ScopedTypeVariables #-}

module Utility((<$$>), leftToMaybe, groupByKey) where

import qualified Data.Map.Strict as M
import Data.Foldable(foldl')

leftToMaybe :: (Either a b) -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe _ = Nothing

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

groupByKey :: forall a b t . (Ord b, Foldable t) => (a -> b) -> t a -> M.Map b [a]
groupByKey f l = foldl' (flip r) M.empty l where
    g :: a -> M.Map b [a]
    g a = M.singleton (f a) [a]
    r :: a -> M.Map b [a] -> M.Map b [a]
    r a m = M.unionWith (++) m (g a)