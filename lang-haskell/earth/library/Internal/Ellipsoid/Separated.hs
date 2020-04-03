{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Ellipsoid.Separated (separatedZones) where

import Prelude hiding (span)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure ((+:), (-:), (*:), u, unQuantity, recip')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Zone (Zone(..), Radius(..), Bearing(..), radius)
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.LatLng (AzimuthFwd, QLat, Lat(..), QLng, Lng(..), LatLng(..))
import Flight.Earth.Ellipsoid (Ellipsoid(..), polarRadius)
import Internal.ClearlySeparated (clearlySeparatedZones)

boundingBoxSeparated
    :: (Num a, Ord a, Fractional a)
    => Ellipsoid a
    -> Zone a
    -> Zone a
    -> Bool
boundingBoxSeparated ellipsoid (Point xLL) (Cylinder (Radius ry) yLL) =
    boxSeparated  ellipsoid (ry, yLL) xLL
boundingBoxSeparated _ _ _ = False

boxSeparated
    :: (Num a, Ord a, Fractional a)
    => Ellipsoid a
    -> (Quantity a [u| m |], LatLng a [u| rad |])
    -> LatLng a [u| rad |]
    -> Bool
boxSeparated
    ellipsoid@Ellipsoid{equatorialR}
    (r', LatLng (Lat yLat, Lng yLng))
    (LatLng (xLLx, xLLy)) =
        xLo || xHi || yLo || yHi
    where
        polarR = polarRadius ellipsoid

        rLat :: Quantity _ [u| rad |]
        rLat = (r' *: recip' polarR) *: [u| 1 rad |]

        Radius rEquatorial = equatorialR
        rLng :: Quantity _ [u| rad |]
        rLng = (r' *: recip' rEquatorial) *: [u| 1 rad |]

        xLo :: Bool
        xLo = xLat' < MkQuantity (negate 1) 

        xHi :: Bool
        xHi = xLat' > MkQuantity 1

        yLo :: Bool
        yLo = xLng' < MkQuantity (negate 1) 

        yHi :: Bool
        yHi = xLng' > MkQuantity 1

        xZero :: Quantity _ [u| rad |]
        xZero = yLat -: rLat

        yZero :: Quantity _ [u| rad |]
        yZero = yLng -: rLng

        xScale :: Quantity _ [u| rad |]
        xScale = (yLat +: rLat) -: (yLat -: rLat)

        yScale :: Quantity _ [u| rad |]
        yScale = (yLng +: rLng) -: (yLng -: rLng)

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
    => Ellipsoid a
    -> AzimuthFwd a
    -> SpanLatLng a
    -> Zone a
    -> Zone a
    -> Bool

separated _ _ _ x@(Point _) y@(Point _) =
    x /= y

separated ellipsoid az span x y@(Point _) =
    separated ellipsoid az span y x

separated ellipsoid _ span x@(Point _) y@(Cylinder r _) =
    boundingBoxSeparated ellipsoid x y || d > ry
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

separated _ _ _ (Point _) (Line Nothing _ _) =
    error "A line's azimuth should be set."

separated ellipsoid azimuthFwd span x@(Point xc) y@(Line (Just (Bearing θ)) r yc) =
    boundingBoxSeparated ellipsoid x y || not (d < ry && overLine)
    where
        (Radius ry) = r
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]
        overLine = fromMaybe False $ do
            az <- azimuthFwd yc xc
            let (MkQuantity δ) = θ -: az
            return $ sin (realToFrac δ :: Double) < 0

separated _ _ span x@(Point _) y =
    d > ry
    where
        (Radius ry) = radius y
        (TaskDistance d) = edgesSum $ distancePointToPoint span [x, y]

-- | Consider cylinders separated if one fits inside the other or if they don't
-- touch.
separated _ _ span xc@(Cylinder (Radius xR) x) yc@(Cylinder (Radius yR) y)
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

separated _ _ span x y =
    clearlySeparatedZones span x y

-- | Are the control zones separated? This a prerequisite to being able to work
-- out the distance between zones. The one exception is coincident cylinders
-- with different radii. Here the difference in radii is considered to be the
-- distance between them. This will be seen where the smaller concentric cylinder
-- marks the launch and the larger one, as an exit cylinder, marks the start of the
-- speed section.
separatedZones
    :: (Real a, Fractional a)
    => Ellipsoid a
    -> AzimuthFwd a
    -> SpanLatLng a
    -> [Zone a]
    -> Bool
separatedZones ellipsoid azFwd span xs =
    and $ zipWith (separated ellipsoid azFwd span) xs (tail xs)
