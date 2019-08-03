module Flight.Earth.ZoneShape.Rational (PointOnRadial, onLine) where

import qualified Data.Number.FixedFunctions as F
import Data.UnitsOfMeasure
    ((+:), (-:), u, abs', fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (Angle(..), deg90, deg270)
import Flight.LatLng.Rational (Epsilon(..))
import Flight.LatLng (LatLng(..))
import Flight.Zone (Bearing(..), Radius(..), center, radius)
import Flight.Zone.Cylinder (ZonePoint(..))

type PointOnRadial
    = LatLng Rational [u| rad |]
    -> Bearing (Quantity Rational [u| rad |])
    -> Radius (Quantity Rational [u| m |])
    -> LatLng Rational [u| rad |]

onLine
    :: Epsilon
    -> PointOnRadial
    -> Maybe (Quantity Rational [u| rad |])
    -> ([ZonePoint Rational], [b]) -> ([ZonePoint Rational], [b])
onLine _ _ Nothing xs = xs
onLine e mkPt (Just theta) (xs, cs) =
    unzip
    [ let delta = angleDiff b theta in (ontoLine e mkPt theta delta x, c)
    | x@ZonePoint{radial = Bearing b} <- xs
    | c <- cs
    ]

angleDiff
    :: (Real a, Fractional a, Show a)
    => Quantity a [u| rad |]
    -> Quantity a [u| rad |]
    -> Quantity a [u| rad |]
angleDiff b theta =
    delta''
    where
        delta = toRational' . abs' $ b -: theta
        delta' = normalize delta
        delta'' = fromRational' delta'

ontoLine
    :: Epsilon
    -> PointOnRadial
    -> Quantity Rational [u| rad |]
    -> Quantity Rational [u| rad |]
    -> ZonePoint Rational
    -> ZonePoint Rational
ontoLine (Epsilon eps) mkPt θ δ x@ZonePoint{sourceZone = z}
    -- NOTE: When δ < 90 then the candidate intercept with the circle is to the
    -- right of the trackline between zone centers.
    | δ <= deg90 =
        let d = F.sin eps angle
            b = θ +: deg90
            r' = MkQuantity $ d * r
        in
            x{point = mkPt o (Bearing b) (Radius r')}

    -- NOTE: When δ > 270 then the candidate intercept with the circle is to
    -- the left of the trackline between zone centers.
    | δ >= deg270 =
        let d = negate $ F.sin eps angle
            b = θ -: deg90
            r' = MkQuantity $ d * r
        in
            x{point = mkPt o (Bearing b) (Radius r')}

    | otherwise = x
    where
        o = center z
        Radius (MkQuantity r) = radius z
        (MkQuantity angle) = δ
