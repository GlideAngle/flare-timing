{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Span.Rational () where

import Data.UnitsOfMeasure ((/:), u, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Data.Number.FixedFunctions as F

import Flight.LatLng.Rational (Epsilon(..), defEps)
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Zone (Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))
import Flight.Geodesy.Rational ()
import Flight.Task (AngleCut(..))
import Flight.ShortestPath (GeoPath(..))

import Flight.Span.Sliver (GeoSliver(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)

fromR :: QTaskDistance Rational [u| m |] -> QTaskDistance Double [u| m |]
fromR (TaskDistance d) = TaskDistance . fromRational' $ d

instance (Real a, Fractional a, GeoPath Rational a) => GeoSliver Rational a where
    angleCut :: Trig Rational a => Earth Rational -> AngleCut Rational
    angleCut _ =
        AngleCut
            { sweep =
                let (Epsilon e) = defEps in
                ArcSweep . Bearing . MkQuantity $ 2 * F.pi e
            , nextSweep =
                \x@AngleCut{sweep = ArcSweep (Bearing b)} ->
                    x{sweep = ArcSweep . Bearing $ b /: 2}
            }

    fromZones :: Trig Rational a => Earth Rational -> Zones -> [TaskZone Rational]
    fromZones x = zonesToTaskZones $ azimuthFwd @Rational @Rational x
