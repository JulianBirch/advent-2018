{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}


module Utility((<$$>), leftToMaybe, groupByKey, atD, atD2) where

import qualified Data.Map.Strict as M
import Data.Foldable(foldl')
import Control.Lens(Lens', lens)

leftToMaybe :: (Either a b) -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe _ = Nothing

infixr 8 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

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

atD2 :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Lens' (M.Map k1 (M.Map k2 v)) v
atD2 k1 k2 v = (atD k1 M.empty) . (atD k2 v)

-- Like !! but decent diagnostics when it fails
(!.!) :: (Show a) => [a] -> Int -> a
list !.! n | n < length list = list !! n
           | otherwise = error ("Failed to lookup " ++ (show n) ++ " in " ++ (show list))
