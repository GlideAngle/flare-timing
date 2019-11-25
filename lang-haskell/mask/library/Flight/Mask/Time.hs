module Flight.Mask.Time (GeoTime(..)) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Kml (MarkedFixes(..))
import Flight.Comp (Task(..))
import Flight.Zone.Raw (Give)
import Flight.Score (PilotTime(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Mask.Tag (GeoTag(..))

class GeoTag g a => GeoTime g a where
    timeFlown
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> Task k
        -> MarkedFixes
        -> Maybe (PilotTime (Quantity Double [u| h |]))
