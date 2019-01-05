{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveTraversable, DerivingStrategies #-}

module Vector (
    Vector2D(..), Vector3D(..), x, y
) where

import Prelude hiding ((+), (*))
import Algebra
import Control.Lens.TH (makeLenses)
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (mempty, mappend)

data Vector2D u = Vector2D { 
    _x :: u, 
    _y :: u 
} deriving stock (Show, Ord, Eq, Functor, Foldable, Traversable)

makeLenses ''Vector2D

{-# INLINE addVector2 #-}
addVector2 :: (Additive a) => Vector2D a -> Vector2D a -> Vector2D a 
addVector2 Vector2D { _x = x1, _y = y1 } (Vector2D { _x = x2, _y = y2 }) = 
    Vector2D { _x = x1 + x2, _y = y1 + y2 }

instance (Additive a) => Additive (Vector2D a) where
    (+) = addVector2

instance (Abelian a) => Abelian (Vector2D a)

instance (Additive a) => Semigroup (Vector2D a) where
    (<>) = addVector2

instance (Additive a, Num a) => Monoid (Vector2D a) where
    mappend = addVector2
    mempty = Vector2D { _x = 0, _y = 0 }
    
instance (Semiring a) => LeftModule' a (Vector2D a) where
    sr .* m = (* sr) <$> m
    
instance (Semiring a) => RightModule' a (Vector2D a) where
    m *. sr = (sr *) <$> m

data Vector3D a = Vector3D { p :: a, q :: a, r :: a } 
    deriving stock (Show, Eq, Functor, Foldable, Traversable)

{-# INLINE addVector3 #-}
addVector3 :: (Additive a) => Vector3D a -> Vector3D a -> Vector3D a 
addVector3 (Vector3D { p = p1, q = q1, r = r1 }) (Vector3D { p = p2, q = q2, r = r2 }) = 
    Vector3D { p = p1+p2, q = q1+q2, r = r1+r2 }

instance (Additive a) => Additive (Vector3D a) where
    (+) = addVector3

instance (Additive a) => Semigroup (Vector3D a) where
    (<>) = addVector3
    
instance (Additive a, Num a) => Monoid (Vector3D a) where
    mempty = Vector3D { p = 0, q = 0, r = 0 }
    mappend = addVector3

instance (Semiring a) => LeftModule' a (Vector3D a) where
    sr .* m = fmap (sr *) m

instance (Semiring a) => RightModule' a (Vector3D a) where
    m *. sr = fmap (* sr) m