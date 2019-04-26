module Flight.Mask.Tag
    ( FnTask
    , FnIxTask
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , MadeZones(..)
    , countFixes
    , madeZones
    , tagZones
    , launched
    , madeGoal
    , started
    , trimOrdLists
    , nullFlying
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import qualified Data.List as List (span)
import Data.Maybe (listToMaybe)
import Data.List (nub, group, elemIndex, findIndex)
import Data.List.Split (chop)
import Control.Lens ((^?), element)
import Control.Arrow (first)
import Control.Monad (join)

import Flight.Clip (FlyingSection, FlyCut(..), FlyClipping(..))
import Flight.LatLng (AzimuthFwd)
import Flight.Distance (SpanLatLng)
import Flight.Kml (Latitude(..), Longitude(..), MarkedFixes(..), secondsToUtc)
import qualified Flight.Kml as Kml
    (LatLngAlt(..), Fix, FixMark(..), Seconds(..))
import Flight.Track.Cross
    ( ZoneCross(..), ZoneTag(..)
    , Seconds(..), TrackFlyingSection(..), RetroActive(..)
    )
import Flight.Track.Time (ZoneIdx(..))
import Flight.Comp (IxTask(..), Task(..), TaskStop(..), Zones(..))
import Flight.Units ()
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , TaskZone(..)
    , OrdCrossing(..)
    , slice
    , fixToPoint
    , fixFromFix
    )
import Flight.Mask.Internal.Cross
    ( isStartExit
    , crossingPredicates
    , crossingSelectors
    , tickedZones
    , entersSeq
    , exitsSeq
    , reindex
    )
import Flight.Mask.Interpolate (TagInterpolate(..), crossingTag)

nullFlying :: TrackFlyingSection
nullFlying =
    TrackFlyingSection
        { loggedFixes = Nothing
        , flyingFixes = Nothing
        , scoredFixes = Nothing
        , loggedSeconds = Nothing
        , flyingSeconds = Nothing
        , scoredSeconds = Nothing
        , loggedTimes = Nothing
        , flyingTimes = Nothing
        , scoredTimes = Nothing
        }

-- | A masking produces a value from a task and tracklog fixes.
type FnTask k a = Task k -> MarkedFixes -> a
type FnIxTask k a = [Task k] -> IxTask -> MarkedFixes -> a

newtype PilotTrackFixes = PilotTrackFixes Int deriving Show

countFixes :: MarkedFixes -> PilotTrackFixes
countFixes MarkedFixes{fixes} =
    PilotTrackFixes $ length fixes

-- | A pilot has launched if their tracklog has distinct fixes.
launched :: FnTask k Bool
launched _ MarkedFixes{fixes} =
    not . null . nub $ fixes

started
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> (Zones -> [TaskZone a])
    -> FnTask k Bool
started az span fromZones Task{speedSection, zones} MarkedFixes{fixes} =
    case slice speedSection (fromZones zones) of
        [] ->
            False

        z : _ ->
            let ez = exitsSeq az span z (fixToPoint <$> fixes)
            in case ez of
                 Right (ZoneExit _ _) : _ ->
                     True

                 _ ->
                     False

madeGoal
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> (Zones -> [TaskZone a])
    -> FnTask k Bool
madeGoal az span fromZones Task{zones} MarkedFixes{fixes} =
    let zs = fromZones zones in
    case reverse zs of
        [] ->
            False

        z : _ ->
            let ez = entersSeq az span z (fixToPoint <$> fixes)
            in case ez of
                 Left (ZoneEntry _ _) : _ ->
                     True

                 _ ->
                     False

-- | Prove from the fixes and mark that the crossing exits.
prove :: [Kml.Fix] -> UTCTime -> ZoneIdx -> ZoneIdx -> [Bool] -> Maybe ZoneCross
prove fixes mark0 i@(ZoneIdx i') j@(ZoneIdx j') bs = do
    fixM <- fixes ^? element i'
    fixN <- fixes ^? element j'
    let fs = [f i fixM, f j fixN]
    return ZoneCross { crossingPair = fs
                     , inZone = bs
                     }
    where
        f = fixFromFix mark0

tagZones
    :: (Real b, Fractional b, TagInterpolate a b)
    => a
    -> [TaskZone b]
    -> [Maybe ZoneCross]
    -> [Maybe ZoneTag]
tagZones f zs cs =
    [ join $ g z <$> c
    | z <- zs
    | c <- cs
    ]
    where
        g z c@ZoneCross{crossingPair, inZone} =
            case (crossingPair, inZone) of
                ([x, y], [a, b]) -> do
                    i <- crossingTag f z (x, y) (a, b)
                    return $ ZoneTag{inter = i, cross = c}

                _ -> Nothing

stationary :: Kml.LatLngAlt a => a -> a -> Bool
stationary x y =
    -- TODO: Account for different logging rates.
    ay > ax - 1 && ay < ax + 1
    &&
    yLat > xLat - e && yLat < xLat + e
    &&
    yLng > xLng - e && yLng < xLng + e
    where
        ax = Kml.altGps x
        ay = Kml.altGps y

        Latitude xLat = Kml.lat x
        Latitude yLat = Kml.lat y
        Longitude xLng = Kml.lng x
        Longitude yLng = Kml.lng y

        e = 0.00001

-- | To work out when a pilot is flying, chop their track log into segments
-- that are close to one another, within ± 1m altitude or within ± 1/10,000th
-- of a degree of latitude or longitude.
--
-- SEE: https://en.wikipedia.org/wiki/Decimal_degrees
--
-- [3, 2, 2, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 
-- Lots of 1s elided and then we get to the end of the flight, moving the
-- glider, packing up, taking a ride in the retrieve vehicle back to the
-- airfield.
--
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 2, 5, 1, 1, 2, 1, 2, 2, 1, 6, 3, 1, 5, 1, 1, 1, 4, 4,
-- 3, 1, 5, 1, 4, 1, 5, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 4, 1, 1, 1, 2, 1, 3,
-- 1, 1, 3, 2, 3, 2, 1, 2, 1, 1, 1, 2, 1, 2, 3, 5, 1, 2, 3, 1, 6, 1, 1, 1, 5,
-- 1, 3, 1, 1, 1, 4, 1, 7, 1, 2, 1, 1, 3, 28, 3, 5, 1, 19, 1, 4, 1, 7, 1, 3, 1,
-- 5, 1, 11, 1, 10, 1, 2, 1, 9, 1, 3, 2, 3, 7, 9, 7, 2, 7, 2, 1, 1, 1, 1, 5, 1,
-- 4, 1, 1, 1, 9, 1, 71, 2, 1, 2, 1, 1, 1, 1, 2, 3, 8, 1, 1, 1, 1, 1, 1, 1, 13,
-- 2, 1, 1, 8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 1, 1, 3, 1, 1, 2, 1, 4, 1,
-- 3, 2, 3, 1, 5, 1, 5, 1, 8, 1, 1, 1, 4, 1, 5, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2,
-- 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 68, 1, 2, 2, 14, 1, 8, 1, 3, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 18, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 15, 2, 7, 4, 13, 1, 3, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 15, 1, 1, 1, 1, 1, 2, 1, 1,
-- 1, 2, 1, 2, 1, 1, 9, 1, 1, 1, 20, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2,
-- 1, 1, 1, 1, 1, 5, 2, 1, 2, 20, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2,
-- 1, 3, 1, 2, 1, 4, 3, 2, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1, 1, 1, 1, 5, 1,
-- 1, 1, 1, 2, 1, 2, 1, 1, 5, 1, 1, 2, 1, 2, 3, 1, 1, 3, 1, 2, 1]
--
-- Then grouping this list, we get;
--
-- [[3], [2, 2], [1], [5], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- ...
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
-- [2], [5], [1, 1], [2], [1], [2, 2], [1], [6], [3], [1], [5], [1, 1, 1], [4,
-- 4], [3], [1], [5], [1], [4], [1], [5], [1, 1, 1, 1], [2], [1, 1, 1, 1, 1],
-- [2], [4], [1, 1, 1], [2], [1], [3], [1, 1], [3], [2], [3], [2], [1], [2],
-- [1, 1, 1], [2], [1], [2], [3], [5], [1], [2], [3], [1], [6], [1, 1, 1], [5],
-- [1], [3], [1, 1, 1], [4], [1], [7], [1], [2], [1, 1], [3], [28], [3], [5],
-- [1], [19], [1], [4], [1], [7], [1], [3], [1], [5], [1], [11], [1], [10],
-- [1], [2], [1], [9], [1], [3], [2], [3], [7], [9], [7], [2], [7], [2], [1, 1,
-- 1, 1], [5], [1], [4], [1, 1, 1], [9], [1], [71], [2], [1], [2], [1, 1, 1,
-- 1], [2], [3], [8], [1, 1, 1, 1, 1, 1, 1], [13], [2], [1, 1], [8], [1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [6], [1, 1], [3], [1, 1], [2], [1], [4], [1], [3],
-- [2], [3], [1], [5], [1], [5], [1], [8], [1, 1, 1], [4], [1], [5], [1], [7],
-- [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [3], [1, 1, 1], [3], [1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [2], [3], [2], [1], [2], [1, 1, 1, 1, 1, 1], [2],
-- [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [68], [1], [2, 2],
-- [14], [1], [8], [1], [3], [1], [5], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [18],
-- [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [4], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1], [15], [2], [7], [4], [13], [1], [3], [9], [1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
-- [15], [1, 1, 1, 1, 1], [2], [1, 1, 1], [2], [1], [2], [1, 1], [9], [1, 1,
-- 1], [20], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1], [5],
-- [2], [1], [2], [20], [2], [1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1], [2, 2], [1, 1, 1, 1, 1,
-- 1], [2], [1], [3], [1], [2], [1], [4], [3], [2], [1, 1, 1, 1, 1], [3], [2],
-- [1], [2], [1, 1, 1, 1, 1, 1], [5], [1, 1, 1, 1], [2], [1], [2], [1, 1], [5],
-- [1, 1], [2], [1], [2], [3], [1, 1], [3], [1], [2], [1]]
--
-- Then count the lengths of those groups.
--
-- [1, 2, 1, 1, 7110, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 3, 2, 1, 1, 1, 1, 1, 1,
-- 1, 4, 1, 5, 1, 1, 3, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1,
-- 3, 1, 1, 1, 1, 1, 1, 4, 1, 1, 1, 7, 1, 1, 2, 1, 61, 1, 2, 1, 2, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 16, 1, 3, 1, 84, 1, 1, 1, 1, 1,
-- 6, 1, 17, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 36, 1, 88, 1, 17, 1, 1, 1, 1, 1, 1,
-- 1, 1, 328, 1, 5, 1, 3, 1, 1, 1, 2, 1, 3, 1, 138, 1, 5, 1, 1, 1, 1, 1, 1, 2,
-- 1, 19, 1, 7, 2, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 6, 1, 4, 1, 1,
-- 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1]
--
-- The result is the range from the input list that selects these elements.
-- 
-- (5,7115)
--
-- Here's another example of a particularly choppy track log, showing the
-- groupings.
--
-- zss =
-- [[31], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [14], [1, 1, 1,
-- 1, 1, 1, 1, 1], [7], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
-- [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [3], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
-- [2], [1, 1], [2], [1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1], [3],
-- [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [3], [1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [3], [1, 1, 1, 1, 1, 1, 1], [2], [1, 1,
-- 1, 1], [3], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [5], [1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1], [4], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1], [2], [1, 1], [2], [1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [5], [1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1], [3], [1], [4], [1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [7], [1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [3], [1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [6], [1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [3], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], [2], [1,
-- 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]
--
-- zls = [1,1326,1,1,433,1,1,1,2447,1,1,270]
--
-- The above was the result when I only allowed the following gap sequence;
--
-- jumpGap (x : 1 : y : xs)
--
-- When the gap sequence was widened from the above to;
--
-- jumpGap (x : 1 : 1 : 1 : y : xs)
-- jumpGap (x : 1 : 1 : y : xs)
-- jumpGap (x : 1 : y : xs)
--
-- I got;
--
-- zls = [1,4483]
flyingSection :: Kml.LatLngAlt a => [a] -> FlyingSection Int
flyingSection ys =
    if null zls then Nothing else
    case elemIndex (maximum zls) zls of
        Nothing -> Nothing
        -- NOTE: A tally is a count and 1-based whereas an index is 0-based.
        Just 0 -> Just (0, tally 0 - 1)
        Just i -> Just (tally (i - 1) - 1, tally i - 1)
    where
        yss = chop (\xs@(x : _) -> List.span (stationary x) xs) ys
        yls = length <$> yss
        zss = group yls
        zls = jumpGaps $ length <$> zss
        tally i =
            sum $ take (iys + 1) yls
            where
                iys = sum $ take (i + 1) zls

jumpGap :: (Ord a, Num a) => [a] -> ([a], [a])
jumpGap (x : 1 : 1 : 1 : y : xs)
    | x > 1 && y > 1 = ([x, 3, y], xs)
jumpGap (x : 1 : 1 : y : xs)
    | x > 1 && y > 1 = ([x, 2, y], xs)
jumpGap (x : 1 : y : xs)
    | x > 1 && y > 1 = ([x, 1, y], xs)
jumpGap (x : xs) = ([x], xs)
jumpGap [] = ([], [])

secondsRange
    :: Kml.FixMark a
    => [a]
    -> FlyingSection Int
    -> FlyingSection Seconds
secondsRange xs section = do
    (i, j) <- section
    x <- listToMaybe $ take 1 $ drop i xs
    y <- listToMaybe $ take 1 $ drop j xs
    let (Kml.Seconds x') = Kml.mark x
    let (Kml.Seconds y') = Kml.mark y
    return (Seconds x', Seconds y')

timeRange :: UTCTime -> FlyingSection Seconds -> FlyingSection UTCTime
timeRange t =
    fmap $
        \(Seconds i, Seconds j) ->
            ( secondsToUtc t $ Kml.Seconds i
            , secondsToUtc t $ Kml.Seconds j
            )

-- >>>
-- > jumpGaps [4,1,2,1,11,1,2,1,800,1,1087]
--
-- [7,1,14,1,1888]
jumpGaps :: [Int] -> [Int]
jumpGaps xs =
    if ys == xs then ys else jumpGaps ys
    where
        ys = sum <$> chop jumpGap xs

madeZones
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> MarkedFixes
    -> MadeZones
madeZones az span fromZones task mf@MarkedFixes{mark0, fixes} =
    MadeZones
        { flying = flying'
        , selectedCrossings = selected
        , nomineeCrossings = nominees
        }
    where
        flying' =
            maybe
                TrackFlyingSection
                    { loggedFixes = Just len
                    , flyingFixes = flyingIndices
                    , scoredFixes = flyingIndices
                    , loggedSeconds = snd <$> loggedSeconds
                    , flyingSeconds = flyingSeconds
                    , scoredSeconds = flyingSeconds
                    , loggedTimes = loggedTimes
                    , flyingTimes = flyingTimes
                    , scoredTimes = flyingTimes
                    }
                (\(RetroActive t) ->
                    let fc = FlyCut{cut =Just(mark0, t), uncut = mf}
                        FlyCut{uncut = MarkedFixes{fixes = fixes'}} = clipToFlown fc
                        indices' = flyingSection fixes'
                        seconds' = secondsRange fixes' indices'
                        times' = timeRange mark0 seconds'
                    in
                        TrackFlyingSection
                            { loggedFixes = Just len
                            , flyingFixes = flyingIndices
                            , scoredFixes = indices'
                            , loggedSeconds = snd <$> loggedSeconds
                            , flyingSeconds = flyingSeconds
                            , scoredSeconds = seconds'
                            , loggedTimes = loggedTimes
                            , flyingTimes = flyingTimes
                            , scoredTimes = times'
                            })
                retro

        retro = RetroActive . retroactive <$> stopped task

        len = length fixes

        loggedIndices = Just (0, len - 1)
        flyingIndices = flyingSection fixes

        loggedSeconds = secondsRange fixes loggedIndices
        flyingSeconds = secondsRange fixes flyingIndices

        loggedTimes = timeRange mark0 loggedSeconds
        flyingTimes = timeRange mark0 flyingSeconds

        (selected, nominees) =
            scoredCrossings
                az
                span
                fromZones
                task
                mf
                (scoredFixes flying')

-- | Finds the crossings in the scored flying section.
scoredCrossings
    :: (Real a, Fractional a)
    => AzimuthFwd a
    -> SpanLatLng a
    -> (Zones -> [TaskZone a])
    -> Task k
    -> MarkedFixes
    -> FlyingSection Int -- ^ The flying section to score
    -> (SelectedCrossings, NomineeCrossings)
scoredCrossings
    az
    span
    fromZones
    task@Task{zones}
    MarkedFixes{mark0, fixes}
    scoredIndices =
    (selected, nominees)
    where
        fixesScored =
            maybe
                fixes
                (\(m, n) -> take (n - m) $ drop m fixes)
                scoredIndices

        xs' =
            maybe
                xs
                (\(ii, _) -> (fmap . fmap) (reindex ii) xs)
                (first ZoneIdx <$> scoredIndices)

        nominees = NomineeCrossings $ f <$> xs'

        ys :: [[OrdCrossing]]
        ys = trimOrdLists ((fmap . fmap) OrdCrossing xs')

        ys' :: [[Crossing]]
        ys' = (fmap . fmap) unOrdCrossing ys

        selectors :: [[Crossing] -> Maybe Crossing]
        selectors =
            (\x ->
                let b = isStartExit az span fromZones x
                in crossingSelectors b x) task

        prover = proveCrossing fixes mark0

        selected =
            SelectedCrossings
            $ zipWith (selectZoneCross prover) selectors ys'

        fs =
            (\x ->
                let b = isStartExit az span fromZones x
                in crossingPredicates az span b x) task

        xs =
            tickedZones
                fs
                (fromZones zones)
                (fixToPoint <$> fixesScored)

        f :: [Crossing] -> [Maybe ZoneCross]
        f [] = []

        f (Right (ZoneExit m n) : es) =
            p : f es
            where
                p = prove fixes mark0 m n [True, False]

        f (Left (ZoneEntry m n) : es) =
            p : f es
            where
                p = prove fixes mark0 m n [False, True]

selectZoneCross
    :: (Crossing -> Maybe ZoneCross)
    -> ([Crossing] -> Maybe Crossing)
    -> [Crossing]
    -> Maybe ZoneCross
selectZoneCross prover selectCrossing xs = do
    x <- selectCrossing xs
    prover x

-- | If I have three sorted lists xs, ys and zs, discard elements of ys that
-- are greater than the last element of zs, then again filter ys so that each
-- element is greater than the first element of xs.
--
-- The reason for doing the comparison between ys and zs first is that on
-- triangle courses,  the first zone is commonly not part of the speed section.
-- This zone may only have crossings made after goal, at the end of the day's
-- racing. These will have high indices and I want to discard them early on in
-- the trimming.
--
-- It is alright too to end up with a null first list of crossings. This will
-- happen in an aerotow comp when the pilot is towed up from outside the first
-- zone.
--
-- >>>
-- > trimOrdLists [[16],[757,964,1237,1251,1369,7058],[2622,2662],[5324,5329],[23,86,89,91,97,98,103,108,109]]
--
-- [[16],[757,964,1237,1251,1369],[2622,2662],[5324,5329],[]]
--
-- > trimOrdLists [[6514,6519],[753,6254],[3106,3953],[],[6502,6529,6602,6616,6757]]
--
-- [[],[753],[3106,3953],[],[]]
trimToOrder :: Ord a => [a] -> [a] -> [a] -> [a]

trimToOrder _ [] _ = []

trimToOrder [] ys (zMin : _) = filter (< zMin) ys

trimToOrder xs ys zs@(_ : _) =
    case xs' of
        [] -> ys'
        (x : _) -> filter (> x) ys'
    where
        (xs', ys') =
            case reverse zs of
                [] -> ([], [])
                (zMax : _) ->
                    case (findIndex (< zMax) xs, findIndex (< zMax) ys) of
                        (_, Nothing) -> (xs, ys)
                        (Nothing, _) -> ([], filter (< zMax) ys)
                        (Just _, Just _) -> (filter (< zMax) xs, filter (< zMax) ys)

trimToOrder (xMin : _) ys [] = filter (> xMin) ys

trimToOrder [] ys [] = ys

-- | Removes elements of the list of lists so that each list only has elements
-- less than elements of subsequent lists and greater than previous lists.
--
-- >>>
-- > trimOrdLists [[25,4953,4955],[783,809,811,817,820,822,952,4812],[1810,1816],[3778,3781],[30,66,144,145,149,151,153,4950,4960,4965]]
--
-- [[25],[783,809,811,817,820,822,952],[1810,1816],[3778,3781],[4950,4960,4965]]
--
-- > trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
--
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
--
-- In another example a pilot only makes it around some of the course but
-- had flown through the goal cylinder before starting the speed section.
--
-- >>>
-- > trimOrdLists [[29],[276],[],[],[32,67,68,69,78]]
--
-- [[29],[276],[],[],[]]
--
-- > trimOrdLists [[4588,4592],[],[1714,1720],[3539,3546],[4584]]
--
-- [[4588,4592],[],[],[],[]]
--
-- > trimOrdLists [[4588,4592],[30,578,583,721,4400],[1714,1720],[3539,3546],[4584]]
--
-- [[],[30,578,583,721],[1714,1720],[3539,3546],[4584]]
--
-- > trimOrdLists [[294,4714,4720,4724],[1367,4597],[2207,2209],[3914,3920],[300,568,570,572,573,4711]]
--
-- [[294],[1367],[2207,2209],[3914,3920],[4711]]
trimOrdLists :: Ord a => [[a]] -> [[a]]
trimOrdLists ys =
    if ys == ys' then nonNullBlock ys else trimOrdLists ys'
    where
        xs = [] : ys
        zs = drop 1 ys ++ [[]]
        ys' = zipWith3 trimToOrder xs ys zs

-- | Going left to right, as soon as an empty list is encountered, all
-- subsequent lists are made null too.
prependNull :: Int -> [[a]] -> [[a]]
prependNull n xs =
    replicate n [] ++ xs

-- | Going left to right, as soon as an empty list is encountered, all
-- subsequent lists are made null too.
lockNullRight :: [[a]] -> [[a]]
lockNullRight xs =
    take (length xs) $ takeWhile (not . null) xs ++ repeat []

-- | Nulls are kept on the left, then a sequence of non-null lists. On the
-- first occurence of a null list, all further lists are replaced by null lists
-- on the right.
nonNullBlock :: [[a]] -> [[a]]
nonNullBlock xs =
    prependNull (length xs - length ys) ys
    where
        ys = lockNullRight $ dropWhile null xs

proveCrossing :: [Kml.Fix] -> UTCTime -> Crossing -> Maybe ZoneCross
proveCrossing fixes mark0 (Right (ZoneExit m n)) =
    prove fixes mark0 m n [True, False]

proveCrossing fixes mark0 (Left (ZoneEntry m n)) =
    prove fixes mark0 m n [False, True]
