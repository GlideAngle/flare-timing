{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Flight.Zone
    ( Radius(..)
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
    , center
    , radius
    , fromRationalRadius
    , fromRationalZone
    , toRationalRadius
    , toRationalZone
    , realToFracRadius
    , realToFracZone
    , fromRationalLatLng
    , toRationalLatLng
    ) where

import Data.UnitsOfMeasure (u, zero, toRational', fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Number.RoundingFunctions (dpRound)
import Data.Bifunctor.Flip (Flip(..))

import Flight.Units (showRadian, realToFrac')
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))

-- | The radius component of zones that are cylinder-like, and most are in some
-- way.
newtype Radius a = Radius (Quantity a [u| m |]) deriving (Eq, Ord)

instance Real a => Show (Radius a) where
    show (Radius d) = "r = " ++ show dbl
        where
            d' = toRational' d
            Flip rounded = dpRound 3 <$> Flip d'
            MkQuantity dbl = fromRational' rounded :: Quantity Double [u| m |]

-- | The incline component of a conical zone.
newtype Incline = Incline (Quantity Rational [u| rad |]) deriving (Eq, Ord)

-- | The bearing component of a vector zone.
newtype Bearing a = Bearing (Quantity a [u| rad |]) deriving (Eq, Ord)

instance Show Incline where
    show (Incline angle) = "i = " ++ showRadian angle

instance Real a => Show (Bearing a) where
    show (Bearing b) = "r = " ++ showRadian (toRational' b)

-- | A control zone of the task. Taken together these make up the course to fly
-- with start enter and exit cylinders, turnpoint cylinders, goal lines and
-- cylinders.
data Zone a where
    -- | Used to mark the exact turnpoints in the optimized task distance.
    Point :: Eq a => LatLng a [u| rad |] -> Zone a

    -- | Used only in open distance tasks these mark the start and direction of
    -- the open distance.
    Vector :: Eq a
           => Bearing a
           -> LatLng a [u| rad |]
           -> Zone a

    -- | The turnpoint cylinder.
    Cylinder :: Eq a
             => Radius a
             -> LatLng a [u| rad |]
             -> Zone a

    -- | Only used in paragliding, this is the conical end of speed section
    -- used to discourage too low an end to final glides.
    Conical :: Eq a
            => Incline
            -> Radius a
            -> LatLng a [u| rad |]
            -> Zone a

    -- | A goal line perpendicular to the course line.
    Line :: Eq a
         => Radius a
         -> LatLng a [u| rad |]
         -> Zone a

    -- | This control zone is only ever used as a goal for paragliding. It is
    -- a goal line perpendicular to the course line followed by half
    -- a cylinder.
    SemiCircle :: Eq a
               => Radius a
               -> LatLng a [u| rad |]
               -> Zone a

deriving instance Eq (Zone a)

fromRationalRadius :: Fractional a => Radius Rational -> Radius a
fromRationalRadius (Radius r) =
    Radius $ fromRational' r

toRationalRadius :: Real a => Radius a -> Radius Rational
toRationalRadius (Radius r) =
    Radius $ toRational' r

realToFracRadius :: (Real a, Fractional b) => Radius a -> Radius b
realToFracRadius (Radius r) =
    Radius $ realToFrac' r

fromRationalLat :: Fractional a => Lat Rational u -> Lat a u
fromRationalLat (Lat x) =
    Lat $ fromRational' x

fromRationalLng :: Fractional a => Lng Rational u -> Lng a u
fromRationalLng (Lng x) =
    Lng $ fromRational' x

fromRationalLatLng :: Fractional a => LatLng Rational u -> LatLng a u
fromRationalLatLng (LatLng (lat, lng)) =
    LatLng (fromRationalLat lat, fromRationalLng lng)

toRationalLat :: Real a => Lat a u -> Lat Rational u
toRationalLat (Lat x) =
    Lat $ toRational' x

toRationalLng :: Real a => Lng a u -> Lng Rational u
toRationalLng (Lng x) =
    Lng $ toRational' x

toRationalLatLng :: Real a => LatLng a u -> LatLng Rational u
toRationalLatLng (LatLng (lat, lng)) =
    LatLng (toRationalLat lat, toRationalLng lng)

realToFracLat :: (Real a, Fractional b) => Lat a u -> Lat b u
realToFracLat (Lat x) =
    Lat $ realToFrac' x

realToFracLng :: (Real a, Fractional b) => Lng a u -> Lng b u
realToFracLng (Lng x) =
    Lng $ realToFrac' x

realToFracLatLng :: (Real a, Fractional b) => LatLng a u -> LatLng b u
realToFracLatLng (LatLng (lat, lng)) =
    LatLng (realToFracLat lat, realToFracLng lng)

fromRationalZone :: (Eq a, Fractional a) => Zone Rational -> Zone a
fromRationalZone (Point x) =
    Point $ fromRationalLatLng x

fromRationalZone (Vector (Bearing b) x) =
    Vector (Bearing $ fromRational' b) (fromRationalLatLng x)

fromRationalZone (Cylinder r x) =
    Cylinder (fromRationalRadius r) (fromRationalLatLng x)

fromRationalZone (Conical i r x) =
    Conical i (fromRationalRadius r) (fromRationalLatLng x)

fromRationalZone (Line r x) =
    Line (fromRationalRadius r) (fromRationalLatLng x)

fromRationalZone (SemiCircle r x) =
    SemiCircle (fromRationalRadius r) (fromRationalLatLng x)

toRationalZone :: Real a => Zone a -> Zone Rational
toRationalZone (Point x) =
    Point $ toRationalLatLng x

toRationalZone (Vector (Bearing b) x) =
    Vector (Bearing $ toRational' b) (toRationalLatLng x)

toRationalZone (Cylinder r x) =
    Cylinder (toRationalRadius r) (toRationalLatLng x)

toRationalZone (Conical i r x) =
    Conical i (toRationalRadius r) (toRationalLatLng x)

toRationalZone (Line r x) =
    Line (toRationalRadius r) (toRationalLatLng x)

toRationalZone (SemiCircle r x) =
    SemiCircle (toRationalRadius r) (toRationalLatLng x)

realToFracZone :: (Real a, Eq b, Fractional b) => Zone a -> Zone b
realToFracZone (Point x) =
    Point $ realToFracLatLng x

realToFracZone (Vector (Bearing (MkQuantity b)) x) =
    Vector (Bearing (MkQuantity $ realToFrac b)) (realToFracLatLng x)

realToFracZone (Cylinder r x) =
    Cylinder (realToFracRadius r) (realToFracLatLng x)

realToFracZone (Conical i r x) =
    Conical i (realToFracRadius r) (realToFracLatLng x)

realToFracZone (Line r x) =
    Line (realToFracRadius r) (realToFracLatLng x)

realToFracZone (SemiCircle r x) =
    SemiCircle (realToFracRadius r) (realToFracLatLng x)

-- | The effective center point of a zone.
center :: Zone a -> LatLng a [u| rad |]
center (Point x) = x
center (Vector _ x) = x
center (Cylinder _ x) = x
center (Conical _ _ x) = x
center (Line _ x) = x
center (SemiCircle _ x) = x

-- | The effective radius of a zone.
radius :: Num a => Zone a -> Radius a
radius (Point _) = Radius zero
radius (Vector _ _) = Radius zero
radius (Cylinder r _) = r
radius (Conical _ r _) = r
radius (Line r _) = r
radius (SemiCircle r _) = r

newtype Deadline = Deadline Integer deriving (Eq, Ord, Show)
newtype TimeOfDay = TimeOfDay Rational deriving (Eq, Ord, Show)
newtype Interval = Interval Rational deriving (Eq, Ord, Show)

data StartGates
    = StartGates
        { open :: TimeOfDay
        , intervals :: [Interval]
        } deriving Show

data Task a
    = Task
        { zones :: [Zone a]
        , startZone :: Int
        , endZone :: Int
        , startGates :: StartGates
        , deadline :: Maybe Deadline
        }
