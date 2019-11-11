module Internal.CylinderOutline.Rational (Circum, getClose) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (Angle(..))
import Flight.LatLng (LatLng(..))
import Flight.LatLng.Rational (Epsilon(..))
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , toRationalZone
    )
import Flight.Zone.Path (distancePointToPoint)
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng)
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , orbit
    , radial
    , point
    , sourceZone
    )

type Circum
    = Epsilon
    -> LatLng Rational [u| rad |]
    -> QRadius Rational [u| m |]
    -> TrueCourse Rational
    -> LatLng Rational [u| rad |]

getClose
    :: Epsilon
    -> SpanLatLng Rational
    -> Circum
    -> Zone Rational
    -> LatLng Rational [u| rad |] -- ^ The center point.
    -> Rational -- ^ The limit radius.
    -> Tolerance Rational
    -> Int -- ^ How many tries.
    -> QRadius Rational [u| m |] -- ^ How far from the center.
    -> (TrueCourse Rational -> LatLng Rational [u| rad |]) -- ^ A point from the origin on this radial
    -> TrueCourse Rational -- ^ The true course for this radial.
    -> (ZonePoint Rational, TrueCourse Rational)
getClose epsilon g circum zone' ptCenter limitRadius spTolerance trys (Radius (MkQuantity offset)) f x@(TrueCourse tc)
    | trys <= 0 = (zp', x)
    | unTolerance spTolerance <= 0 = (zp', x)
    | limitRadius <= unTolerance spTolerance = (zp', x)
    | otherwise =
        case d `compare` limitRadius of
             EQ ->
                 (zp', x)

             GT ->
                 let offset' =
                         offset - (d - limitRadius) * 105 / 100

                     f' =
                         circumR (Radius (MkQuantity $ limitRadius + offset'))

                 in
                     getClose
                         epsilon
                         g
                         circum
                         zone'
                         ptCenter
                         limitRadius
                         spTolerance
                         (trys - 1)
                         (Radius (MkQuantity offset'))
                         f'
                         x

             LT ->
                 if d > toRational (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     let offset' =
                             offset + (limitRadius - d) * 94 / 100

                         f' =
                             circumR (Radius (MkQuantity $ limitRadius + offset'))
                     in
                         getClose
                             epsilon
                             g
                             circum
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (Radius (MkQuantity offset'))
                             f'
                             x
    where
        circumR = circum epsilon ptCenter

        y = f x

        zp' :: ZonePoint Rational
        zp' = ZonePoint
                { sourceZone = toRationalZone zone'
                , point = y
                , radial = Bearing $ normalize tc
                , orbit = Radius yr
                }

        pts = [Point ptCenter, Point y]

        (TaskDistance yr@(MkQuantity d)) =
            edgesSum $ distancePointToPoint g pts
