module Flight.Cylinder.Edge (sample) where

import Prelude hiding (span)
import Data.UnitsOfMeasure (zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , Bearing(..)
    )
import Flight.Cylinder.Rational (circumSample)
import Flight.Cylinder.Sample (SampleParams, ZonePoint(..))

-- | Generate sample points for a zone. These get added to the graph to work out
-- the shortest path.
sample :: (Real a, Fractional a)
       => SampleParams a
       -> Bearing
       -> Maybe (ZonePoint a)
       -> Zone a
       -> [ZonePoint a]
sample _ _ _ px@(Point x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample _ _ _ px@(Vector _ x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample sp b zs z@Cylinder{} = fst $ circumSample sp b zs z
sample sp b zs z@Conical{} = fst $ circumSample sp b zs z
sample sp b zs z@Line{} = fst $ circumSample sp b zs z
sample sp b zs z@SemiCircle{} = fst $ circumSample sp b zs z
