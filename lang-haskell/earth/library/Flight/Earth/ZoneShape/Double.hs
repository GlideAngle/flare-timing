module Flight.Earth.ZoneShape.Double (PointOnRadial, onLine) where

import Data.UnitsOfMeasure ((+:), (-:), u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (Angle(..), deg90, deg270)
import Flight.LatLng (LatLng(..))
import Flight.Zone (Bearing(..), Radius(..), center, radius)
import Flight.Zone.Cylinder (ZonePoint(..))

type PointOnRadial
    = LatLng Double [u| rad |]
    -> Bearing (Quantity Double [u| rad |])
    -> Radius (Quantity Double [u| m |])
    -> LatLng Double [u| rad |]

onLine
    :: PointOnRadial
    -> Maybe (Quantity Double [u| rad |])
    -> ([ZonePoint Double], [b]) -> ([ZonePoint Double], [b])
onLine _ Nothing xs = xs
onLine mkPt (Just theta) (xs, cs) =
    unzip
    [ let delta = angleDiff b theta in (ontoLine mkPt theta delta x, c)
    | x@ZonePoint{radial = Bearing b} <- xs
    | c <- cs
    ]

-- |
-- >>> round' $ angleDiff [u| 0 rad |] [u| 0 rad |]
-- [u| 0 rad |]
--
-- >>> round' $ angleDiff [u| 0 rad |] (pi *: [u| 2 rad |])
-- [u| 0 rad |]
--
-- >>> angleDiff (pi *: [u| 2 rad |]) [u| 0 rad |]
-- [u| 6.283185307179586 rad |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| 0 deg |]
-- [u| 0 deg |]
--
-- >>> round' $ angleDiffDeg [u| 360 deg |] [u| 0 deg |]
-- [u| 360 deg |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| 360 deg |]
-- [u| 0 deg |]
--
-- >>> round' $ angleDiffDeg [u| -360 deg |] [u| 0 deg |]
-- [u| 0 deg |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| -360 deg |]
-- [u| 360 deg |]
--
-- >>> round' $ angleDiffDeg [u| 90 deg |] [u| 0 deg |]
-- [u| 90 deg |]
--
-- >>> round' $ angleDiffDeg [u| 180 deg |] [u| 0 deg |]
-- [u| 180 deg |]
--
-- >>> round' $ angleDiffDeg [u| 270 deg |] [u| 0 deg |]
-- [u| 270 deg |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| 90 deg |]
-- [u| 270 deg |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| 180 deg |]
-- [u| 180 deg |]
--
-- >>> round' $ angleDiffDeg [u| 0 deg |] [u| 270 deg |]
-- [u| 90 deg |]
angleDiff
    :: (Real a, Fractional a, Show a)
    => Quantity a [u| rad |]
    -> Quantity a [u| rad |]
    -> Quantity a [u| rad |]
angleDiff b theta =
    delta''
    where
        delta = toRational' $ b -: theta
        delta' = normalize delta
        delta'' = fromRational' delta'

ontoLine
    :: PointOnRadial
    -> Quantity Double [u| rad |]
    -> Quantity Double [u| rad |]
    -> ZonePoint Double
    -> ZonePoint Double
ontoLine mkPt θ δ x@ZonePoint{sourceZone = z}
    -- NOTE: When δ < 90 then the candidate intercept with the circle is to the
    -- right of the trackline between zone centers.
    | δ <= deg90 =
        let d = sin angle
            b = θ +: deg90
            r' = MkQuantity $ d * r
        in
            x{point = mkPt o (Bearing b) (Radius r')}

    -- NOTE: When δ > 270 then the candidate intercept with the circle is to
    -- the left of the trackline between zone centers.
    | δ >= deg270 =
        let d = negate $ sin angle
            b = θ -: deg90
            r' = MkQuantity $ d * r
        in
            x{point = mkPt o (Bearing b) (Radius r')}

    | otherwise = x
    where
        o = center z
        Radius (MkQuantity r) = radius z
        (MkQuantity angle) = δ

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
--
-- >>> :{
-- angleDiffDeg
--    :: (Real a, Fractional a, Show a)
--    => Quantity a [u| deg |]
--    -> Quantity a [u| deg |]
--    -> Quantity a [u| deg |]
-- angleDiffDeg x y = convert $ angleDiff (convert x) (convert y)
-- :}
--
-- >>> :{
-- round' :: (RealFrac a, Integral b) => Quantity a u -> Quantity b u
-- round' (MkQuantity x) = MkQuantity (round x)
-- :}
