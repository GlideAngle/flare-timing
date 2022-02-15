{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Mask.Interpolate
    ( GeoTagInterpolate(..)
    , linearInterpolate
    ) where

import Data.UnitsOfMeasure ((+:), (-:), (*:), u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (LatLng(..))
import Flight.Zone.Cylinder (SampleParams(..))
import qualified Flight.Track.Cross as Cg (Fix(..))
import Flight.Track.Cross (InterpolatedFix(..))
import Flight.Task (Zs(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Span.Sliver (GeoSliver(..))
import Flight.Mask.Internal.Zone (TaskZone(..))

class GeoSliver g a => GeoTagInterpolate g a where
    interpolate
        :: Trig g a
        => Earth g
        -> SampleParams g
        -> TaskZone g
        -> LatLng g [u| rad |]
        -> LatLng g [u| rad |]
        -> Zs [LatLng g [u| rad |]]

    fractionate
        :: Trig g a
        => Earth g
        -> Zs [LatLng g [u| rad |]]
        -> Maybe (LatLng g [u| rad |], g)

    -- | Given two points on either side of a zone, what is the crossing tag.
    crossingTag
        :: Trig g a
        => Earth g
        -> SampleParams g
        -> TaskZone g
        -> (Cg.Fix, Cg.Fix)
        -> (Bool, Bool)
        -> Maybe InterpolatedFix

-- TODO: Find out why this cannot be implemented in terms of Quantity a u
linearInterpolate
    :: Num a
    => a
    -> Quantity a [u| m |]
    -> Quantity a [u| m |]
    -> Quantity a [u| m |]
linearInterpolate frac q0 q1 =
    ((MkQuantity frac :: Quantity _ [u| 1 |]) *: (q1 -: q0)) +: q0
