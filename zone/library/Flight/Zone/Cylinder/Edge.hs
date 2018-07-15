module Flight.Zone.Cylinder.Edge (CircumSample, sample) where

import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , QBearing
    , Bearing(..)
    )
import Flight.Zone.Cylinder.Sample (SampleParams, ZonePoint(..), TrueCourse)

-- | The type of function that samples points on the circumference of a circle.
type CircumSample a
    = SampleParams a
    -> QBearing a [u| rad |]
    -> Maybe (ZonePoint a)
    -> Zone a
    -> ([ZonePoint a], [TrueCourse a])

-- | Generate sample points for a zone. These get added to the graph to work out
-- the shortest path.
sample :: (Real a, Fractional a)
       => CircumSample a
       -> SampleParams a
       -> QBearing a [u| rad |]
       -> Maybe (ZonePoint a)
       -> Zone a
       -> [ZonePoint a]
sample _ _ _ _ px@(Point x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample _ _ _ _ px@(Vector _ x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample circumSample sp b zs z@Cylinder{} = fst $ circumSample sp b zs z
sample circumSample sp b zs z@Conical{} = fst $ circumSample sp b zs z
sample circumSample sp b zs z@Line{} = fst $ circumSample sp b zs z
sample circumSample sp b zs z@Circle{} = fst $ circumSample sp b zs z
sample circumSample sp b zs z@SemiCircle{} = fst $ circumSample sp b zs z
