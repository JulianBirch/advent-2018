{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE FunctionalDependencies, ScopedTypeVariables #-}

module Square where

import Prelude hiding ()
-- import qualified Prelude as P
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)
import Data.Proxy (Proxy(Proxy))
import Algebra hiding ((+), (*))
import qualified Numeric.Additive.Class as A ((+))

import Vector (Vector2D(..))

data SquareDirection = SquareE | SquareS | SquareW | SquareN  
    deriving (Enum, Bounded)
data Square8Direction = Square8E | Square8SE | Square8S | Square8SW 
        | Square8W | Square8NW | Square8N | Square8NE  
    deriving (Enum, Bounded)

class (Enum a, Bounded a) => Direction a where
    angleOfFirstVectorInDegrees :: (Floating f) => Proxy a -> f
    numberOfDirections :: Proxy a -> Int
    left :: a -> a
    right :: a -> a

instance Direction SquareDirection where
    angleOfFirstVectorInDegrees = const (0-45)
    numberOfDirections = const 4
    left SquareE = SquareN
    left SquareS = SquareE
    left SquareW = SquareS
    left SquareN = SquareW
    right SquareN = SquareE 
    right SquareE = SquareS 
    right SquareS = SquareW 
    right SquareW = SquareN 

instance Direction Square8Direction where
    angleOfFirstVectorInDegrees = const (0-22.5)
    numberOfDirections = const 8
    left Square8E = Square8NE
    left Square8NE = Square8N
    left Square8N = Square8NW
    left Square8NW = Square8W
    left Square8W = Square8SW
    left Square8SW = Square8S
    left Square8S = Square8SE
    left Square8SE = Square8S
    right Square8E = Square8SE
    right Square8SE = Square8S
    right Square8S = Square8SW
    right Square8SW = Square8W
    right Square8W = Square8NW
    right Square8NW = Square8N
    right Square8N = Square8NE
    right Square8NE = Square8N
    
newtype Square d a = Square (Vector2D a) deriving stock (Show, Eq)

deriving via (Vector2D a) instance forall d . (Additive a) => Additive (Square d a) 
deriving via (Vector2D a) instance forall d . (Abelian a) => Abelian (Square d a) 
deriving via (Vector2D a) instance forall d . (Additive a) => Semigroup (Square d a) 
deriving via (Vector2D a) instance forall d . (Additive a, Num a) => Monoid (Square d a) 
deriving via (Vector2D a) instance forall d . (Semiring a) => LeftModule' a (Square d a) 
deriving via (Vector2D a) instance forall d . (Semiring a) => RightModule' a (Square d a)

-- Square is implicitly a module now

-- ripped off astro 

-- | Convert from degrees to radians
toRadians :: Floating a => a -> a
toRadians deg = deg * pi/180

unitCircleVector :: (Floating f) => f -> Square d f
unitCircleVector f =  Square $ Vector2D { _x = cos f, _y = sin f }

class (Direction d, Semigroup p, LeftModule' r p) => GridVector d r p | p -> d, p -> r where
    move1 :: d -> p -> p
    move1 d = ((unitVector d) <>)
    moveN :: d -> r -> p -> p
    moveN d n = ((n .* (unitVector d)) <>)
    unitVector :: d -> p

instance (Num a, Semiring a) => GridVector SquareDirection a (Square SquareDirection a) where
    unitVector SquareE = Square Vector2D { _x = 1, _y = 0 }
    unitVector SquareN = Square Vector2D { _x = 0, _y = 1 }
    unitVector SquareW = Square Vector2D { _x = -1, _y = 0 }
    unitVector SquareS = Square Vector2D { _x = 0, _y = -1 }
-- corresponds to hex_corner, returns the first corner anti-clockwise from the directon specified

gridCorner :: forall d f . (Direction d, Floating f, Semiring f) => Square d f -> f -> d -> Square d f
gridCorner centre size direction = centre A.+ x where
    pr :: Proxy d
    pr = Proxy    
    x = size .* unitCircleVector r 
    r = (toRadians $ angleOfFirstVectorInDegrees pr) + (2.0 * pi/(fromIntegral $ numberOfDirections pr) * (fromIntegral $ fromEnum direction))

