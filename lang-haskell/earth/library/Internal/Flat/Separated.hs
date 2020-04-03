{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Flat.Separated (separatedZones) where

import Prelude hiding (span)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure ((-:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (easting, northing)

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), Bearing(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.LatLng (AzimuthFwd, LatLng(..))
import Internal.Flat.Projected.Internal (zoneToProjectedEastNorth)
import Internal.ClearlySeparated (clearlySeparatedZones)

boundingBoxSeparated
    :: (Num a, Ord a, Real a, Fractional a)
    => Zone a
    -> Zone a
    -> Bool
boundingBoxSeparated (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated (ry, yLL) xLL
boundingBoxSeparated _ _ = False

boxSeparated
    :: (Num a, Ord a, Real a, Fractional a)
    => (Quantity a [u| m |], LatLng a [u| rad |])
    -> LatLng a [u| rad |]
    -> Bool
boxSeparated (MkQuantity r', yLL) xLL =
    case ( zoneToProjectedEastNorth (Point xLL)
         , zoneToProjectedEastNorth (Point yLL)
         ) of
        (Right x, Right y) ->
            dN > r && dE > r
            where
                dN = yN - xN
                dE = yE - xE

                xN = HC.northing x
                yN = HC.northing y

                xE = HC.easting x
                yE = HC.easting y

        _ -> error "Cannot project lat/lng to UTM easting/northing"
    where
        r = realToFrac r'

separated
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> Zone a
    -> Zone a
    -> Bool

separated _ _ x@(Point _) y@(Point _) =
    x /= y

separated az span x y@(Point _) =
    separated az span y x

separated _ span x@(Point _) y@(Cylinder r _) =
    boundingBoxSeparated x y || d > ry
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

separated _ _ (Point _) (Line Nothing _ _) =
    error "A line's azimuth should be set."

separated azimuthFwd span x@(Point xc) y@(Line (Just (Bearing θ)) r yc) =
    boundingBoxSeparated x y || not (d < ry && overLine)
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]
        overLine = fromMaybe False $ do
            az <- azimuthFwd yc xc
            let (MkQuantity δ) = θ -: az
            return $ sin (realToFrac δ :: Double) < 0

separated _ span x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated _ span xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
    | x == y = xR /= yR
    | dxy + minR < maxR = True
    | otherwise = clearlySeparatedZones span xc yc
    where
        (TaskDistance (MkQuantity dxy)) =
            edgesSum
            $ distancePointToPoint
                span
                ([Point x, Point y] :: [Zone _])

        (MkQuantity minR) = min xR yR
        (MkQuantity maxR) = max xR yR

separated _ span x y =
    clearlySeparatedZones span x y

-- | Are the control zones separated? This a prerequisite to being able to work
-- out the distance between zones. The one exception is coincident cylinders
-- with different radii. Here the difference in radii is considered to be the
-- distance between them. This will be seen where the smaller concentric cylinder
-- marks the launch and the larger one, as an exit cylinder, marks the start of the
-- speed section.
separatedZones
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> [Zone a]
    -> Bool
separatedZones azFwd span xs =
    and $ zipWith (separated azFwd span) xs (tail xs)
