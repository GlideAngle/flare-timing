{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Span.Double () where

import Data.UnitsOfMeasure ((/:))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Bearing(..), ArcSweep(..))
import Flight.Zone.MkZones (Zones)
import Flight.Zone.Raw (Give)
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))
import Flight.Geodesy.Double ()
import Flight.Task (AngleCut(..))
import Flight.ShortestPath (GeoPath(..))

import Flight.Span.Sliver (GeoSliver(..))
import Flight.Mask.Internal.Zone (TaskZone, zonesToTaskZones)

instance (RealFloat a, GeoPath Double a) => GeoSliver Double a where
    angleCut :: Trig Double a => Earth Double -> AngleCut Double
    angleCut _ =
        AngleCut
            { sweep = ArcSweep . Bearing . MkQuantity $ 2 * pi
            , nextSweep =
                \x@AngleCut{sweep = ArcSweep (Bearing b)} ->
                    x{sweep = ArcSweep . Bearing $ b /: 2}
            }

    fromZones :: Trig Double a => Earth Double -> Maybe Give -> Zones -> [TaskZone Double]
    fromZones e g = zonesToTaskZones g $ azimuthFwd @Double @Double e
