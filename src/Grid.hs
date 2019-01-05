{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
{-# LANGUAGE DerivingVia, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Grid where

import Prelude hiding ((+), (*))
import qualified Prelude as P
import Data.Semigroup (Semigroup)
import Data.Proxy (Proxy(Proxy))
import Control.Lens (Iso, iso, Lens')
import Control.Lens.Operators ((^.), (%~))
import Numeric.Algebra.Class hiding ((.*), (*.))
import Numeric.Additive.Class
import Vector
import Algebra

newtype Square a = Square (Vector2D a) deriving stock (Show, Eq)

deriving via (Vector2D a) instance (Additive a) => Additive (Square a) 
deriving via (Vector2D a) instance (Abelian a) => Abelian (Square a) 
deriving via (Vector2D a) instance (Additive a) => Semigroup (Square a) 
deriving via (Vector2D a) instance (Additive a, Num a) => Monoid (Square a) 
deriving via (Vector2D a) instance (Semiring a) => LeftModule' a (Square a) 
deriving via (Vector2D a) instance (Semiring a) => RightModule' a (Square a)

deriving via (Vector2D a) instance (Ord a) => Ord (Square a)

-- deriving via (Vector2D a) instance forall d . (Additive a) => Additive (Square d a) 

-- Square is implicitly a module now

data SquareDirection = SquareE | SquareS | SquareW | SquareN  
    deriving (Enum, Bounded, Show, Eq, Ord)
data PointyHexDirection = PointyE | PointySE | PointySW | PointyW | PointyNW | PointyNE
    deriving (Enum, Bounded, Show, Eq, Ord)
data FlatHexDirection = FlatSE | FlatSW | FlatS | FlatN | FlatNW | FlatNE
    deriving (Enum, Bounded, Show, Eq, Ord)

class (Enum a, Bounded a) => Direction a where
    angleOfFirstVectorInDegrees :: (Floating f) => Proxy a -> f
    numberOfDirections :: Proxy a -> Int
    opposite :: a -> a

instance Direction SquareDirection where
    angleOfFirstVectorInDegrees = const (0-45)
    numberOfDirections = const 4
    opposite SquareE = SquareW
    opposite SquareN = SquareS
    opposite SquareW = SquareE
    opposite SquareS = SquareN

instance Direction PointyHexDirection where
    angleOfFirstVectorInDegrees = const (0-30)
    numberOfDirections = const 6
    opposite PointyE = PointyW
    opposite PointySE = PointyNW
    opposite PointySW = PointyNE
    opposite PointyW = PointyE
    opposite PointyNE = PointySW
    opposite PointyNW = PointySE

instance Direction FlatHexDirection where
    angleOfFirstVectorInDegrees = const 0
    numberOfDirections = const 6
    opposite FlatSE = FlatNW
    opposite FlatSW = FlatNE
    opposite FlatS = FlatN
    opposite FlatNE = FlatSW
    opposite FlatNW = FlatSE
    opposite FlatN = FlatS

-- ripped off astro 

-- | Convert from degrees to radians
toRadians :: Floating a => a -> a
toRadians deg = deg P.* pi/180

unitCircleVector :: (Floating f) => f -> Square f
unitCircleVector f =  Square $ Vector2D { _x = cos f, _y = sin f }

-- corresponds to hex_corner, returns the first corner anti-clockwise from the directon specified

gridCorner :: forall d f . (Direction d, Floating f, Semiring f) => Square f -> f -> d -> Square f
gridCorner centre size direction = centre + x where
    pr :: Proxy d
    pr = Proxy    
    x = size .* unitCircleVector r 
    r = (toRadians $ angleOfFirstVectorInDegrees pr) P.+ (2.0 P.* pi/(fromIntegral $ numberOfDirections pr) P.* (fromIntegral $ fromEnum direction))

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

class (Direction d, Monoid p) => Vector p d | p -> d where
    move :: d -> p -> p

instance Vector (Square Int) SquareDirection where
    move SquareN (Square (Vector2D x y)) = Square $ Vector2D x (y+1)
    move SquareS (Square (Vector2D x y)) = Square $ Vector2D x (y-1)
    move SquareE (Square (Vector2D x y)) = Square $ Vector2D (x+1) y
    move SquareW (Square (Vector2D x y)) = Square $ Vector2D (x-1) y

{-# INLINE axialHexCubeIso #-}
axialHexCubeIso :: forall d a b . (Num b) => Iso (HexCube d a) (HexCube d b) (Axial d a) (Axial d b)
axialHexCubeIso = iso hexCubeToAxial axialToHexCube where
  -- HexCubeToAxial :: HexCube d a -> Axial d a
  hexCubeToAxial (HexCube Vector3D { p = p, r = r }) = Axial Vector2D { _x = p , _y = r }
  -- axialToHexCube :: (Num a) => Axial d a -> HexCube d a
  axialToHexCube (Axial Vector2D { _x = x, _y = y }) = HexCube Vector3D { p = x, r = y, q = 0-x-y}

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

{-# INLINE axialOffsetIso #-}
axialOffsetIso :: forall a b d o . (Integral a, Integral b, OffsetCalculator o, HexDirection d) 
    => Iso (Axial d a) (Axial d b) (Offset d o a) (Offset d o b)
axialOffsetIso = iso axialToOffset offsetToAxial where
    po :: Proxy o
    po = Proxy
    pd :: Proxy d
    pd = Proxy
    adj :: forall n . (Integral n) => Vector2D n -> n
    adj p = adjustment po $ p ^. otherAxis pd
    convert :: forall n . (Integral n) => (n -> n -> n) -> Vector2D n -> Vector2D n
    convert f p = (offsetAxis pd) %~ (flip f (adj p)) $ p
    axialToOffset (Axial p) = Offset $ convert (P.+) p
    offsetToAxial (Offset p) = Axial $ convert (P.-) p

