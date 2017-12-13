{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Inputs
    ( MadeGoalLookup(..)
    , ArrivalRankLookup(..)
    , PilotTimeLookup(..)
    , StartEnd
    , readTags
    , tagMadeGoal
    , tagArrivalRank
    , tagPilotTime
    ) where

import Data.Time.Clock (UTCTime)
import Data.List (find, elemIndex)
import Data.Maybe (listToMaybe)
import Control.Monad (join)
import Control.Monad.Except (ExceptT(..), lift)
import Control.Lens ((^?), element)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.TrackLog (IxTask(..))
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp (TagFile(..), SpeedSection, Pilot(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..))
import Flight.Mask (slice)
import Flight.Track.Cross (Fix(time))

type TaggingLookup a = Pilot -> SpeedSection -> IxTask -> Kml.MarkedFixes -> Maybe a

newtype MadeGoalLookup = MadeGoalLookup (Maybe (TaggingLookup Bool))
newtype ArrivalRankLookup = ArrivalRankLookup (Maybe (TaggingLookup Int))
newtype PilotTimeLookup = PilotTimeLookup (Maybe (TaggingLookup StartEnd))

type StartEnd = (UTCTime, UTCTime)

readTags :: TagFile -> ExceptT String IO Tagging
readTags (TagFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

tagMadeGoal :: Either String Tagging -> MadeGoalLookup
tagMadeGoal (Left _) = MadeGoalLookup Nothing
tagMadeGoal (Right x) = MadeGoalLookup (Just $ madeGoal x)

tagPilotTime :: Either String Tagging -> PilotTimeLookup
tagPilotTime (Left _) = PilotTimeLookup Nothing
tagPilotTime (Right x) = PilotTimeLookup (Just $ timeElapsed x)

tagArrivalRank :: Either String Tagging -> ArrivalRankLookup
tagArrivalRank (Left _) = ArrivalRankLookup Nothing
tagArrivalRank (Right x) = ArrivalRankLookup (Just $ arrivalRank x)

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

madeGoal :: Tagging
         -> Pilot
         -> SpeedSection
         -> IxTask
         -> Kml.MarkedFixes
         -> Maybe Bool
madeGoal _ _ Nothing _ _ = Nothing
madeGoal x pilot speedSection (IxTask i) _ =
    case tagging x ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just xs ->
            join
            $ madeGoalPilot speedSection
            <$> find (\(PilotTrackTag p _) -> p == pilot) xs

madeGoalPilot :: SpeedSection -> PilotTrackTag -> Maybe Bool
madeGoalPilot _ (PilotTrackTag _ Nothing) = Nothing
madeGoalPilot Nothing _ = Nothing
madeGoalPilot speedSection (PilotTrackTag _ (Just TrackTag{zonesTag})) =
    const True <$> bs
    where
        zs :: [Maybe _] = slice speedSection zonesTag
        bs :: Maybe [_] = sequence zs

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
