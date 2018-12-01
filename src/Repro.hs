{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DerivingVia, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repro where

import Prelude hiding ((+))

class (Additive a) where
    (+) :: a -> a -> a

data Vector2D u = Vector2D { 
    x :: u, 
    y :: u 
}

addVector2 :: (Additive a) => Vector2D a -> Vector2D a -> Vector2D a 
addVector2 Vector2D { x = x1, y = y1 } (Vector2D { x = x2, y = y2 }) = 
    Vector2D { x = x1 + x2, y = y1 + y2 }

instance (Additive a) => Additive (Vector2D a) where
    (+) = addVector2

newtype Phantom1 d a = Phantom1 (Vector2D a) --Axial 

deriving via (Vector2D a) instance forall d . (Additive a) => (Additive (Phantom1 d a))

newtype Via a b = Via a

class IsoEvidence a b where
    convertTo :: a -> b
    convertFrom :: b -> a

instance forall a b . (IsoEvidence a b, Additive b) => (Additive (Via a b)) where
    (Via x) + (Via y) = Via $ convertFrom $ (convertTo x :: b) + (convertTo y :: b)  

newtype Phantom2 d a = Phantom2 (Phantom1 d a) --Axial 

deriving via (Vector2D a) instance (Additive a) => (Additive (Phantom2 d a))

