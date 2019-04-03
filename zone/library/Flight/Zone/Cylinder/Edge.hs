module Flight.Zone.Cylinder.Edge (CircumSample, sample) where

import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , ArcSweep(..)
    , Bearing(..)
    )
import Flight.Zone.Cylinder.Sample (SampleParams, ZonePoint(..), TrueCourse)

-- | The type of function that samples points on the circumference of a circle.
type CircumSample a
    = SampleParams a
    -- ^ Control of the iterations and tolerance.
    -> ArcSweep a [u| rad |]
    -- ^ The angle either side of the point's angle defining an arc of the
    -- circle's circumference from which to sample points.
    -> Maybe (ZonePoint a)
    -- ^ This point will be in the middle of the arc of the sample.
    -> Maybe (Zone a)
    -- ^ The previous zone along the course.
    -> Zone a
    -- ^ The zone from which we're trying to find points on an arc.
    -> ([ZonePoint a], [TrueCourse a])
    -- ^ The points found and the angle of each.

-- | Generate sample points for a zone. These get added to the graph to work out
-- the shortest path.
sample
    :: (Real a, Fractional a)
    => CircumSample a
    -> SampleParams a
    -> ArcSweep a [u| rad |]
    -> Maybe (ZonePoint a)
    -> Maybe (Zone a)
    -> Zone a
    -> [ZonePoint a]
sample _ _ _ _ _ px@(Point x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample _ _ _ _ _ px@(Vector _ x) = [ZonePoint px x (Bearing zero) (Radius (MkQuantity 0))]
sample circumSample sp b arc0 zM zN@Cylinder{} = fst $ circumSample sp b arc0 zM zN
sample circumSample sp b arc0 zM zN@Conical{} = fst $ circumSample sp b arc0 zM zN
sample circumSample sp b arc0 zM zN@Line{} = fst $ circumSample sp b arc0 zM zN
sample circumSample sp b arc0 zM zN@Circle{} = fst $ circumSample sp b arc0 zM zN
sample circumSample sp b arc0 zM zN@SemiCircle{} = fst $ circumSample sp b arc0 zM zN
