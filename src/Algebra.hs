{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}

module Algebra ((+), (*), (.*), (*.), 
    Semiring, Additive, Abelian, 
    LeftModule', RightModule', Module',
    Via(..), IsoEvidence, convertFrom, convertTo) where

import Prelude hiding ((+), (*))
import Numeric.Algebra.Class (Semiring, (*))
import Numeric.Additive.Class (Additive, (+), Abelian)
import Data.Coerce ()
import Data.Function (on)
import Data.Semigroup ((<>))
import Data.Monoid (mempty, mappend)

class (Semiring r, Additive m) => LeftModule' r m where
    (.*) :: r -> m -> m
  
class (Semiring r, Additive m) => RightModule' r m where
    (*.) :: m -> r -> m

class (LeftModule' r m, RightModule' r m) => Module' r m where -- this needs UndecideableInstances 

instance (LeftModule' r m, RightModule' r m) => Module' r m 

newtype Via a b = Via a

class IsoEvidence a b where
    convertTo :: a -> b
    convertFrom :: b -> a

combine :: forall a b . (IsoEvidence a b) => (b -> b -> b) -> (Via a b) -> (Via a b) -> (Via a b)
combine c (Via x) (Via y) = Via $ convertFrom $ on c convertTo x y

instance forall a b . (IsoEvidence a b, Semigroup b) => (Semigroup (Via a b)) where
    (<>) = combine (<>)

instance forall a b . (IsoEvidence a b, Monoid b) => (Monoid (Via a b)) where
    mappend = combine mappend
    mempty = Via $ convertFrom (mempty :: b)

instance forall a b . (IsoEvidence a b, Additive b) => (Additive (Via a b)) where
    (+) = combine (+)

instance forall a b . (IsoEvidence a b, Abelian b) => (Abelian (Via a b)) where

instance forall a b r . (IsoEvidence a b, LeftModule' r b) => (LeftModule' r (Via a b)) where
    r .* (Via m) = Via $ convertFrom $ r .* (convertTo m :: b)

instance forall a b r. (IsoEvidence a b, RightModule' r b) => (RightModule' r (Via a b)) where
    (Via m) *. r = Via $ convertFrom $ (convertTo m :: b) *. r
