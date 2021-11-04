module Flight.Zone.Cylinder.Edge
    ( CircumSample
    , sample
    , sampleAngles
    ) where

import Data.UnitsOfMeasure ((-:), (+:), u, unQuantity, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units.Angle (deg90)
import Flight.Zone
    ( Zone(..)
    , Radius(..)
    , ArcSweep(..)
    , Bearing(..)
    )
import Flight.Zone.Cylinder.Sample
    (Samples(..), SampleParams(..), ZonePoint(..), TrueCourse(..))

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

-- | Generate radial angles from the zone center point from which to take
-- samples and return the azimuth to the previous zone's center.
sampleAngles
    :: (Fractional a, Real a)
    => a -- ^ The angle Pi.
    -> Samples -- ^ How many samples to take.
    -> ArcSweep a u -- ^ How far to sweep the arc of samples.
    -> Maybe (ZonePoint a) -- ^ The best point from the previous sample iteration.
    -> Maybe c -- ^ The previous zone.
    -> Zone a -- ^ The zone we're generating angles for.
    -> (Maybe (Quantity a [u| rad |]), [TrueCourse a])
sampleAngles pi' samples (ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN =
    (fmap . fmap) (TrueCourse . MkQuantity) $
    case (zoneM, zoneN) of
        (Nothing, _) -> (Nothing, cs)
        (Just _, Point{}) -> (Nothing, cs)
        (Just _, Vector{}) -> (Nothing, cs)
        (Just _, Cylinder{}) -> (Nothing, cs)
        (Just _, Conical{}) -> (Nothing, cs)
        (Just _, Line Nothing _ _) -> (Nothing, cs)
        (Just _, Line (Just (Bearing az)) _ _) ->
            -- NOTE: For a line we don't want to miss a likely local
            -- minimum where the line intersects the circle so let's
            -- add those true courses explicitly now at 90° and 270°
            -- from the azimuth.
            (Just az,) $
            if bearing < 2 * pi'
               then cs
               else
                    unQuantity (az +: deg90)
                    : unQuantity (az -: deg90)
                    : cs

        (Just _, Circle{}) -> (Nothing, cs)
        (Just _, SemiCircle{}) -> (Nothing, cs)
    where
        nNum = unSamples samples
        half = nNum `div` 2
        step = bearing / fromIntegral nNum
        mid = maybe 0 (\ZonePoint{radial = Bearing (MkQuantity b)} -> b) arc0

        cs =
                let lhs = [mid - fromIntegral n * step | n <- [1 .. half]]
                    rhs = [mid + fromIntegral n * step | n <- [1 .. half]]
                -- NOTE: The reverse of the LHS is not needed for correct
                -- operation but it helps when tracing.
                in reverse lhs ++ (mid : rhs)
