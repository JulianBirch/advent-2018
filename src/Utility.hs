module Utility((<$$>), leftToMaybe) where

leftToMaybe :: (Either a b) -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe _ = Nothing

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap