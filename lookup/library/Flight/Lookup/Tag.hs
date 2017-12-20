{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Lookup.Tag
    ( ArrivalRankLookup(..)
    , PilotTimeLookup(..)
    , PilotTagLookup(..)
    , TickedLookup(..)
    , StartEnd
    , tagArrivalRank
    , tagPilotTime
    , tagPilotTag
    , tagTicked
    ) where

import Data.Time.Clock (UTCTime)
import Data.List (find, elemIndex)
import Data.Maybe (catMaybes, listToMaybe, isJust)
import Control.Monad (join)
import Control.Lens ((^?), element)
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp (IxTask(..), SpeedSection, Pilot(..))
import Flight.Track.Tag
    (Tagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..))
import Flight.Mask (Ticked, RaceSections(..), slice, section)
import Flight.Track.Cross (Fix(fix, time))

type TaggingLookup a =
    Pilot -> SpeedSection -> IxTask -> Kml.MarkedFixes -> Maybe a

newtype ArrivalRankLookup = ArrivalRankLookup (Maybe (TaggingLookup Int))
newtype PilotTimeLookup = PilotTimeLookup (Maybe (TaggingLookup StartEnd))
newtype PilotTagLookup = PilotTagLookup (Maybe (TaggingLookup [Maybe Fix]))
newtype TickedLookup = TickedLookup (Maybe (TaggingLookup Ticked))

type StartEnd = (UTCTime, UTCTime)

tagTicked :: Either String Tagging -> TickedLookup
tagTicked (Left _) = TickedLookup Nothing
tagTicked (Right x) = TickedLookup (Just $ ticked x)

tagPilotTime :: Either String Tagging -> PilotTimeLookup
tagPilotTime (Left _) = PilotTimeLookup Nothing
tagPilotTime (Right x) = PilotTimeLookup (Just $ timeElapsed x)

tagPilotTag :: Either String Tagging -> PilotTagLookup
tagPilotTag (Left _) = PilotTagLookup Nothing
tagPilotTag (Right x) = PilotTagLookup (Just $ tagged x)

tagArrivalRank :: Either String Tagging -> ArrivalRankLookup
tagArrivalRank (Left _) = ArrivalRankLookup Nothing
tagArrivalRank (Right x) = ArrivalRankLookup (Just $ arrivalRank x)

ticked :: Tagging
       -> Pilot
       -> SpeedSection
       -> IxTask
       -> Kml.MarkedFixes
       -> Maybe Ticked
ticked _ _ Nothing _ _ = Nothing
ticked x pilot speedSection (IxTask i) _ =
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
            -> Pilot
            -> SpeedSection
            -> IxTask
            -> Kml.MarkedFixes
            -> Maybe StartEnd
timeElapsed _ _ Nothing _ _ = Nothing
timeElapsed x pilot speedSection (IxTask i) _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            join
            $ timeElapsedPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

-- | The time of the first and last fix in the list.
startEnd :: [Maybe Fix] -> Maybe StartEnd
startEnd xs = do
    ys <- sequence xs
    start <- listToMaybe $ take 1 ys
    end <- listToMaybe $ take 1 $ reverse ys
    return (time start, time end)

timeElapsedPilot :: SpeedSection -> PilotTrackTag -> Maybe StartEnd
timeElapsedPilot _ (PilotTrackTag _ Nothing) = Nothing
timeElapsedPilot Nothing _ = Nothing
timeElapsedPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    startEnd $ slice speedSection zonesTag

tagged :: Tagging
       -> Pilot
       -> SpeedSection
       -> IxTask
       -> Kml.MarkedFixes
       -> Maybe [Maybe Fix]
tagged _ _ Nothing _ _ = Nothing
tagged x pilot speedSection (IxTask i) _ =
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
            -> Pilot
            -> SpeedSection
            -> IxTask
            -> Kml.MarkedFixes
            -> Maybe Int
arrivalRank _ _ Nothing _ _ = Nothing
arrivalRank x pilot speedSection (IxTask i) _ =
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
