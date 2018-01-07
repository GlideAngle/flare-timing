{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Lookup.Tag
    ( TaskTimeLookup(..)
    , ArrivalRankLookup(..)
    , TimeLookup(..)
    , TagLookup(..)
    , TickLookup(..)
    , tagTaskTime
    , tagArrivalRank
    , tagPilotTime
    , tagPilotTag
    , tagTicked
    ) where

import Data.List (find, elemIndex)
import Data.Maybe (catMaybes, listToMaybe, isJust)
import Control.Monad (join)
import Control.Lens ((^?), element)
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp
    ( IxTask(..)
    , Pilot(..)
    , StartEnd(..)
    , StartEndMark
    , FirstLead(..)
    , LastArrival(..)
    , SpeedSection
    )
import Flight.Track.Tag
    (Tagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..)
    , firstLead, lastArrival
    )
import Flight.Mask (Ticked, RaceSections(..), slice, section)
import Flight.Track.Cross (Fix(fix, time))

type TaskTaggingLookup a = IxTask -> SpeedSection -> Maybe a

newtype TaskTimeLookup = TaskTimeLookup (Maybe (TaskTaggingLookup StartEndMark))

type TaggingLookup a = IxTask -> SpeedSection -> Pilot -> Kml.MarkedFixes -> Maybe a

tagTaskTime :: Either String Tagging -> TaskTimeLookup
tagTaskTime = TaskTimeLookup . either (const Nothing) (Just . taskTimeElapsed)

taskTimeElapsed
    :: Tagging
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
newtype TagLookup = TagLookup (Maybe (TaggingLookup [Maybe Fix]))
newtype TickLookup = TickLookup (Maybe (TaggingLookup Ticked))

tagTicked :: Either String Tagging -> TickLookup
tagTicked = TickLookup . either (const Nothing) (Just . ticked)

tagPilotTime :: Either String Tagging -> TimeLookup
tagPilotTime = TimeLookup . either (const Nothing) (Just . timeElapsed)

tagPilotTag :: Either String Tagging -> TagLookup
tagPilotTag = TagLookup . either (const Nothing) (Just . tagged)

tagArrivalRank :: Either String Tagging -> ArrivalRankLookup
tagArrivalRank = ArrivalRankLookup . either (const Nothing) (Just . arrivalRank)

ticked :: Tagging
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
            join
            $ tickedPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

-- | The time of the first and last fix in the list.
tickedZones :: SpeedSection -> [Maybe Fix] -> Ticked
tickedZones speedSection xs =
    RaceSections
        { prolog = f prolog
        , race = f race
        , epilog = f epilog
        }
    where
        f = catMaybes . takeWhile isJust
        RaceSections{..} = section speedSection $ (fmap . fmap) fix xs

tickedPilot :: SpeedSection -> PilotTrackTag -> Maybe Ticked
tickedPilot _ (PilotTrackTag _ Nothing) = Nothing
tickedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    Just $ tickedZones speedSection zonesTag

timeElapsed :: Tagging
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
            join
            $ timeElapsedPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

-- | The time of the first and last fix in the list.
startEnd :: [Maybe Fix] -> Maybe StartEndMark
startEnd xs = do
    ys <- sequence xs
    start <- listToMaybe $ take 1 ys
    end <- listToMaybe $ take 1 $ reverse ys
    return $ StartEnd (time start) (Just $ time end)

timeElapsedPilot :: SpeedSection -> PilotTrackTag -> Maybe StartEndMark
timeElapsedPilot _ (PilotTrackTag _ Nothing) = Nothing
timeElapsedPilot Nothing _ = Nothing
timeElapsedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    startEnd $ slice speedSection zonesTag

tagged :: Tagging
       -> IxTask
       -> SpeedSection
       -> Pilot
       -> Kml.MarkedFixes
       -> Maybe [Maybe Fix]
tagged _ _ Nothing _ _ = Nothing
tagged x (IxTask i) speedSection pilot _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            taggedPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

taggedPilot :: SpeedSection -> PilotTrackTag -> [Maybe Fix]
taggedPilot _ (PilotTrackTag _ Nothing) = []
taggedPilot Nothing _ = []
taggedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    slice speedSection zonesTag

arrivalRank :: Tagging
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
