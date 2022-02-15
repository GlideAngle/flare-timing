module Flight.Lookup.Tag
    ( TaskTimeLookup(..)
    , TaskLeadingLookup(..)
    , ArrivalRankLookup(..)
    , TimeLookup(..)
    , LeadingLookup(..)
    , TagLookup(..)
    , TickLookup(..)
    , tagTaskTime
    , tagTaskLeading
    , tagArrivalRank
    , tagPilotTime
    , tagPilotTag
    , tagTicked
    ) where

import Data.List (find, elemIndex)
import Data.Maybe (catMaybes, listToMaybe, isJust)
import Control.Lens ((^?), element)
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , StartEnd(..)
    , StartEndMark
    , StartEndDown(..)
    , StartEndDownMark
    , FirstLead(..)
    , LastArrival(..)
    )
import Flight.Track.Time (ZoneIdx(..))
import Flight.Track.Tag
    ( CompTagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..)
    , firstLead, lastArrival
    )
import Flight.Mask (Ticked, RaceSections(..), slice, section)
import Flight.Track.Cross (InterpolatedFix(..), ZoneTag(..))

type TaskTaggingLookup a = IxTask -> SpeedSection -> Maybe a

newtype TaskTimeLookup =
    TaskTimeLookup
        (Maybe (TaskTaggingLookup StartEndMark))

newtype TaskLeadingLookup =
    TaskLeadingLookup
        (Maybe (TaskTaggingLookup StartEndDownMark))

type TaggingLookup a = IxTask -> SpeedSection -> Pilot -> Kml.MarkedFixes -> Maybe a

tagTaskTime :: Maybe CompTagging -> TaskTimeLookup
tagTaskTime = TaskTimeLookup . fmap taskTimeElapsed

tagTaskLeading :: Maybe CompTagging -> TaskLeadingLookup
tagTaskLeading = TaskLeadingLookup . fmap taskLeadingTimes

taskLeadingTimes
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Maybe StartEndDownMark
taskLeadingTimes _ _ Nothing = Nothing
taskLeadingTimes x (IxTask i) ss = do
    TrackTime{zonesFirst, zonesLast, lastLanding} <- timing x ^? element (fromIntegral i - 1)
    FirstLead start <- firstLead ss zonesFirst
    let end = (\(LastArrival a) -> a) <$> lastArrival ss zonesLast
    return $ StartEndDown start end lastLanding

taskTimeElapsed
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Maybe StartEndMark
taskTimeElapsed _ _ Nothing = Nothing
taskTimeElapsed x (IxTask i) ss = do
    TrackTime{zonesFirst, zonesLast} <- timing x ^? element (fromIntegral i - 1)
    FirstLead start <- firstLead ss zonesFirst
    let end = (\(LastArrival a) -> a) <$> lastArrival ss zonesLast
    return $ StartEnd start end

newtype ArrivalRankLookup = ArrivalRankLookup (Maybe (TaggingLookup Int))
newtype TimeLookup = TimeLookup (Maybe (TaggingLookup StartEndMark))
newtype LeadingLookup = LeadingLookup (Maybe (TaggingLookup StartEndDownMark))
newtype TagLookup = TagLookup (Maybe (TaggingLookup [Maybe ZoneTag]))
newtype TickLookup = TickLookup (Maybe (TaggingLookup Ticked))

tagTicked :: Maybe CompTagging -> TickLookup
tagTicked = TickLookup . fmap ticked

tagPilotTime :: Maybe CompTagging -> TimeLookup
tagPilotTime = TimeLookup . fmap timeElapsed

tagPilotTag :: Maybe CompTagging -> TagLookup
tagPilotTag = TagLookup . fmap tagged

tagArrivalRank :: Maybe CompTagging -> ArrivalRankLookup
tagArrivalRank = ArrivalRankLookup . fmap arrivalRank

ticked
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Kml.MarkedFixes
    -> Maybe Ticked
ticked _ _ Nothing _ _ = Nothing
ticked x (IxTask i) speedSection pilot _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            tickedPilot speedSection
            =<< find (\(PilotTrackTag p _) -> p == pilot) xs

-- | The time of the first and last fix in the list.
tickedZones :: SpeedSection -> [Maybe ZoneTag] -> Ticked
tickedZones speedSection xs =
    RaceSections
        { prolog = f prolog
        , race = f race
        , epilog = f epilog
        }
    where
        f = fmap ZoneIdx . catMaybes . takeWhile isJust
        RaceSections{..} =
            section speedSection
            $ (fmap . fmap) (round . fixFrac . inter) xs

tickedPilot :: SpeedSection -> PilotTrackTag -> Maybe Ticked
tickedPilot _ (PilotTrackTag _ Nothing) = Nothing
tickedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    Just $ tickedZones speedSection zonesTag

timeElapsed
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Kml.MarkedFixes
    -> Maybe StartEndMark
timeElapsed _ _ Nothing _ _ = Nothing
timeElapsed x (IxTask i) speedSection pilot _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            timeElapsedPilot speedSection
            =<< find (\(PilotTrackTag p _) -> p == pilot) xs

-- | The time of the first and last fix in the list.
startEnd :: [Maybe ZoneTag] -> Maybe StartEndMark
startEnd xs = do
    ys <- sequence xs
    start <- listToMaybe $ take 1 ys
    end <- listToMaybe $ take 1 $ reverse ys
    return $ StartEnd (time . inter $ start) (Just . time . inter $ end)

timeElapsedPilot :: SpeedSection -> PilotTrackTag -> Maybe StartEndMark
timeElapsedPilot _ (PilotTrackTag _ Nothing) = Nothing
timeElapsedPilot Nothing _ = Nothing
timeElapsedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    startEnd $ slice speedSection zonesTag

tagged
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Kml.MarkedFixes
    -> Maybe [Maybe ZoneTag]
tagged _ _ Nothing _ _ = Nothing
tagged x (IxTask i) speedSection pilot _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            taggedPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

taggedPilot :: SpeedSection -> PilotTrackTag -> [Maybe ZoneTag]
taggedPilot _ (PilotTrackTag _ Nothing) = []
taggedPilot Nothing _ = []
taggedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    slice speedSection zonesTag

arrivalRank
    :: CompTagging
    -> IxTask
    -> SpeedSection
    -> Pilot
    -> Kml.MarkedFixes
    -> Maybe Int
arrivalRank _ _ Nothing _ _ = Nothing
arrivalRank x (IxTask i) speedSection pilot _ =
    case timing x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just TrackTime{..} -> arrivalRankPilot pilot speedSection zonesRankPilot

arrivalRankPilot :: Pilot -> SpeedSection -> [[Pilot]] -> Maybe Int
arrivalRankPilot _ Nothing _ = Nothing
arrivalRankPilot p speedSection xss =
    case pss of
        [] -> Nothing
        (ps : _) -> (+ 1) <$> elemIndex p ps
    where
        pss :: [[Pilot]] = reverse $ slice speedSection xss
