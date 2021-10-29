{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Sphere.Separated (separatedZones) where

import Prelude hiding (span)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure ((+:), (-:), (*:), u, unQuantity, recip')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), Bearing(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.LatLng (AzimuthFwd, QLat, Lat(..), QLng, Lng(..), LatLng(..))
import Flight.Earth.Sphere (earthRadius)
import Internal.ClearlySeparated (clearlySeparatedZones)

boundingBoxSeparated
    :: (Num a, Ord a, Fractional a)
    => Zone a
    -> Zone a
    -> Bool
boundingBoxSeparated (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated (ry, yLL) xLL
boundingBoxSeparated _ _ = False

boxSeparated
    :: (Num a, Ord a, Fractional a)
    => (Quantity a [u| m |], LatLng a [u| rad |])
    -> LatLng a [u| rad |]
    -> Bool
boxSeparated
    (r', LatLng (Lat yLat, Lng yLng))
    (LatLng (xLLx, xLLy)) =
        xLo || xHi || yLo || yHi
    where
        Radius rEarth = earthRadius

        -- NOTE: Use *: recip' instead of /: to avoid needing a
        -- Floating constraint that is not available for Rational.
        r :: Quantity _ [u| rad |]
        r = (r' *: recip' rEarth) *: [u| 1 rad |]

        xLo :: Bool
        xLo = xLat' < MkQuantity (negate 1) 

        xHi :: Bool
        xHi = xLat' > MkQuantity 1

        yLo :: Bool
        yLo = xLng' < MkQuantity (negate 1) 

        yHi :: Bool
        yHi = xLng' > MkQuantity 1

        xZero :: Quantity _ [u| rad |]
        xZero = yLat -: r

        yZero :: Quantity _ [u| rad |]
        yZero = yLng -: r

        xScale :: Quantity _ [u| rad |]
        xScale = (yLat +: r) -: (yLat -: r)

        yScale :: Quantity _ [u| rad |]
        yScale = (yLng +: r) -: (yLng -: r)

        xTranslate :: QLat _ [u| rad |] -> QLat _ [u| rad |]
        xTranslate (Lat lat) =
            Lat ((lat -: xZero) *: scale)
            where
                scale :: Quantity _ [u| 1 1 |]
                scale = MkQuantity (unQuantity xScale)

        yTranslate :: QLng _ [u| rad |] -> QLng _ [u| rad |]
        yTranslate (Lng lng) =
            Lng ((lng -: yZero) *: scale)
            where
                scale :: Quantity _ [u| 1 1 |]
                scale = MkQuantity (unQuantity yScale)

        (Lat xLat') = xTranslate xLLx
        (Lng xLng') = yTranslate xLLy

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
            return $ cos (realToFrac δ :: Double) < 0

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
-- distance between them. This will be seen where the smaller concentric
-- cylinder marks the launch and the larger one, as an exit cylinder, marks the
-- start of the speed section.
separatedZones
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> [Zone a]
    -> Bool
separatedZones azFwd span xs =
    and $ zipWith (separated azFwd span) xs (tail xs)
{-# INLINABLE separatedZones #-}
{-# SPECIALIZE
   separatedZones
       :: AzimuthFwd Double
       -> SpanLatLng Double
       -> [Zone Double]
       -> Bool #-}
{-# SPECIALIZE
   separatedZones
       :: AzimuthFwd Rational
       -> SpanLatLng Rational
       -> [Zone Rational]
       -> Bool #-}
