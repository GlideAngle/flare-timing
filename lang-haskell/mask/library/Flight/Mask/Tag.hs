module Flight.Mask.Tag
    ( GeoTag(..)
    , FnTask
    , FnIxTask
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , ExcludedCrossings(..)
    , MadeZones(..)
    , PilotTrackFixes(..)
    , selectZoneCross
    ) where

import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Clip (FlyingSection)
import Flight.Kml (MarkedFixes(..))
import Flight.Track.Cross (ZoneCross(..), ZoneTag(..))
import Flight.Comp (IxTask(..), Task(..))
import Flight.Units ()
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , ExcludedCrossings(..)
    , Crossing
    , TaskZone(..)
    )
import Flight.Mask.Interpolate (GeoTagInterpolate(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

-- | A masking produces a value from a task and tracklog fixes.
type FnTask k a = Task k -> MarkedFixes -> a
type FnIxTask k a = [Task k] -> IxTask -> MarkedFixes -> a

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

class GeoTagInterpolate g a => GeoTag g a where
    started :: Trig g a => Earth g -> FnTask k Bool
    madeGoal :: Trig g a => Earth g -> FnTask k Bool

    -- | The zones that are made without regard to the stopped time of stopped
    -- tasks.
    madeZones :: Trig g a => Earth g -> Task k -> MarkedFixes -> MadeZones

    -- | Finds the crossings in the flying section. All of these crossings may
    -- not be included in the scored section of the flight and will be clipped
    -- later.
    flyingCrossings
        :: Trig g a
        => Earth g
        -> Task k
        -> MarkedFixes
        -> FlyingSection Int -- ^ The fix indices of the flying section
        -> (SelectedCrossings, NomineeCrossings, ExcludedCrossings)

    tagZones
        :: Trig g a
        => Earth g
        -> SampleParams g
        -> [TaskZone g]
        -> [Maybe ZoneCross]
        -> [Maybe ZoneTag]

selectZoneCross
    :: (Crossing -> Maybe ZoneCross)
    -> ([Crossing] -> Maybe Crossing)
    -> [Crossing]
    -> Maybe ZoneCross
selectZoneCross prover selectCrossing xs = do
    x <- selectCrossing xs
    prover x
