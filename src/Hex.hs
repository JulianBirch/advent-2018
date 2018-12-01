{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE DerivingVia, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Hex where

import Prelude hiding ((+), (*))
import qualified Prelude as P
import Square (Direction(..))
import Vector(Vector2D(..), Vector3D(..), x, y)
import Data.Semigroup ()
import Data.Monoid ()
import Data.Proxy (Proxy(Proxy))
import Control.Lens (Lens')
import Control.Lens.Operators ((^.), (%~))
import Algebra 

data PointyHexDirection = PointyE | PointySE | PointySW | PointyW | PointyNW | PointyNE
    deriving (Enum, Bounded)
data FlatHexDirection = FlatSE | FlatS | FlatSW | FlatN | FlatNW | FlatNE
    deriving (Enum, Bounded)

instance Direction PointyHexDirection where
    angleOfFirstVectorInDegrees = const (0-30)
    numberOfDirections = const 6
    
    left PointyE = PointySE
    left PointySE = PointySW
    left PointySW = PointyW
    left PointyW = PointyNW
    left PointyNW = PointyNE
    left PointyNE = PointyE

    right PointyE = PointyNE
    right PointyNE = PointyNW
    right PointyNW = PointyW
    right PointyW = PointySW
    right PointySW = PointySE
    right PointySE = PointyE
    

instance Direction FlatHexDirection where
    angleOfFirstVectorInDegrees = const 0
    numberOfDirections = const 6
    
    left FlatSE = FlatS
    left FlatS = FlatSW
    left FlatSW = FlatNW
    left FlatNW = FlatN
    left FlatN = FlatNE
    left FlatNE = FlatSE

    right FlatNE = FlatN
    right FlatN = FlatNW
    right FlatNW = FlatSW
    right FlatSW = FlatS
    right FlatS = FlatSE
    right FlatSE = FlatNE
newtype HexCube d a = HexCube (Vector3D a)
    deriving (Show, Eq) -- Do not derive functor here, you could produce nonsense

deriving via (Vector3D a) instance forall d . (Additive a) => Additive (HexCube d a) 
deriving via (Vector3D a) instance forall d . (Abelian a) => Abelian (HexCube d a) 
deriving via (Vector3D a) instance forall d . (Additive a) => Semigroup (HexCube d a) 
deriving via (Vector3D a) instance forall d . (Additive a, Num a) => Monoid (HexCube d a) 
deriving via (Vector3D a) instance forall d . (Semiring a) => LeftModule' a (HexCube d a) 
deriving via (Vector3D a) instance forall d . (Semiring a) => RightModule' a (HexCube d a)

newtype Axial d a = Axial (Vector2D a) deriving (Show, Eq)

data EvenOffset
data OddOffset

class OffsetCalculator a where
    adjustment :: (Integral n) => Proxy a -> n -> n

instance OffsetCalculator EvenOffset where
    {-# INLINE adjustment #-}
    adjustment _ n = (n P.+ n `mod` 2) `div` 2

instance OffsetCalculator OddOffset where
    {-# INLINE adjustment #-}
    adjustment _ n = (n P.- n `mod` 2) `div` 2

newtype Offset d o a = Offset (Vector2D a) deriving (Show, Eq)

instance (Num a) => (IsoEvidence (HexCube d a) (Axial d a)) where
    convertTo (HexCube Vector3D { p = p, r = r }) = Axial Vector2D { _x = p , _y = r }
    convertFrom (Axial Vector2D { _x = x, _y = y }) = HexCube Vector3D { p = x, r = y, q = 0-x-y}

class (Direction d) => HexDirection d where
    offsetAxis :: Proxy d -> Lens' (Vector2D a) a
    otherAxis :: Proxy d -> Lens' (Vector2D a) a

instance HexDirection PointyHexDirection where
    {-# INLINE offsetAxis #-}
    offsetAxis = const x
    {-# INLINE otherAxis #-}
    otherAxis = const y

instance HexDirection FlatHexDirection where
    {-# INLINE offsetAxis #-}
    offsetAxis = const y
    {-# INLINE otherAxis #-}
    otherAxis = const x

{-# INLINE convert #-}
convert :: forall d o n . (Integral n, OffsetCalculator o, HexDirection d) => Proxy o -> Proxy d -> (n -> n -> n) -> Vector2D n -> Vector2D n  
convert po pd f p = (offsetAxis pd) %~ (flip f (adj p)) $ p where
    adj :: Vector2D n -> n
    adj p = adjustment po $ p ^. otherAxis pd

instance (Integral n, OffsetCalculator o, HexDirection d) => (IsoEvidence (Axial d n) (Offset d o n)) where
    convertFrom (Offset p) = Axial $ convert (Proxy :: Proxy o) (Proxy :: Proxy d) (P.-) p
    convertTo (Axial p) = Offset $ convert (Proxy :: Proxy o) (Proxy :: Proxy d) (P.+) p

deriving via (Via (Axial d n) (Offset d o n)) 
    instance (Additive n, IsoEvidence (Axial d n) (Offset d o n)) => 
        (Semigroup (Offset d o n))

deriving via (Via (Axial d n) (Offset d o n)) 
    instance (Num n, Additive n, IsoEvidence (Axial d n) (Offset d o n)) => 
        (Monoid (Offset d o n))
    
deriving via (Via (Axial d n) (Offset d o n)) 
    instance (Additive n, IsoEvidence (Axial d n) (Offset d o n)) => 
        (Additive (Offset d o n))
    
deriving via (Via (Axial d n) (Offset d o n)) 
    instance (Abelian n, IsoEvidence (Axial d n) (Offset d o n)) => 
        (Abelian (Offset d o n))

deriving via (Via (Axial d n) (Offset d o n)) 
    instance (LeftModule' n (Axial d n), IsoEvidence (Axial d n) (Offset d o n)) => 
        (LeftModule' n (Offset d o n))

deriving via (Via (Axial d n) (Offset d o n)) 
    instance (RightModule' n (Axial d n), IsoEvidence (Axial d n) (Offset d o n)) => 
        (RightModule' n (Offset d o n))
    