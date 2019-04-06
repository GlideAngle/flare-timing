module Flight.Earth.ZoneShape.Double (PointOnRadial, onLine) where

import Data.UnitsOfMeasure
    ((*:), (+:), (-:), u, fromRational', toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (Angle(..), halfPi)
import Flight.LatLng (LatLng(..))
import Flight.Zone (Bearing(..), Radius(..), center, radius)
import Flight.Zone.Cylinder (ZonePoint(..))

type PointOnRadial
    = LatLng Double [u| rad |]
    -> Bearing (Quantity Double [u| rad |])
    -> Radius (Quantity Double [u| m |])
    -> LatLng Double [u| rad |]

deg90 :: (Real a, Fractional a) => Quantity a [u| rad |]
deg90 = halfPi

deg270 :: (Real a, Fractional a) => Quantity a [u| rad |]
deg270 = 3 *: halfPi

onLine
    :: PointOnRadial
    -> Maybe (Quantity Double [u| rad |])
    -> ([ZonePoint Double], [b]) -> ([ZonePoint Double], [b])
onLine _ Nothing xs = xs
onLine mkPt (Just theta) (xs, cs) =
    unzip $
    [
        let delta = angleDiff b theta in
        if delta < deg90 || delta > deg270
           then (ontoLine mkPt theta delta x, c)
           else (x, c)
    | x@ZonePoint{radial = Bearing b} <- xs
    | c <- cs
    ]

-- |
-- >>> angleDiffDeg [u| 0 deg |] [u| 0 deg |]
-- [u| 0.0 deg |]
--
-- >>> angleDiffDeg [u| 360 deg |] [u| 0 deg |]
-- [u| 0.0 deg |]
--
-- >>> angleDiffDeg [u| 0 deg |] [u| 360 deg |]
-- [u| 0.0 deg |]
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
ontoLine mkPt theta delta x@ZonePoint{sourceZone = z}
    | delta < deg90 =
        let d = sin angle
            b' = theta +: deg90
            r' = MkQuantity $ d * r
            yPt = mkPt o (Bearing b') (Radius r')
        in
            x{point = yPt}

    | delta > deg270 =
        let d = negate $ sin angle
            b' = theta -: deg90
            r' = MkQuantity $ d * r
            yPt = mkPt o (Bearing b') (Radius r')
        in
            x{point = yPt}

    | otherwise = x
    where
        o = center z
        Radius (MkQuantity r) = radius z
        MkQuantity angle = delta

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
-- >>> import Data.UnitsOfMeasure (u, convert)
--
-- >>> :{
-- angleDiffDeg
--    :: (Real a, Fractional a, Show a)
--    => Quantity a [u| deg |]
--    -> Quantity a [u| deg |]
--    -> Quantity a [u| deg |]
-- angleDiffDeg x y = convert $ angleDiff (convert x) (convert y)
-- :}
