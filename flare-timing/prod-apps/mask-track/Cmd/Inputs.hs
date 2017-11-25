{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Inputs
    ( MadeGoal(..)
    , ArrivalRank(..)
    , readTags
    , tagMadeGoal
    , tagArrivalRank
    ) where

import Data.List (find, elemIndex)
import Control.Monad (join)
import Control.Monad.Except (ExceptT(..), lift)
import Control.Lens ((^?), element)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.TrackLog (IxTask(..))
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp (SpeedSection, Pilot(..))
import Flight.Track.Tag (Tagging(..), TrackTime(..), TrackTag(..), PilotTrackTag(..))
import Flight.Mask (slice)

type TaggingLookup a = Pilot -> SpeedSection -> IxTask -> Kml.MarkedFixes -> Maybe a
newtype MadeGoal = MadeGoal (Maybe (TaggingLookup Bool))
newtype ArrivalRank = ArrivalRank (Maybe (TaggingLookup Int))

readTags :: FilePath -> ExceptT String IO Tagging
readTags yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents

tagMadeGoal :: Either String Tagging -> MadeGoal
tagMadeGoal (Left _) = MadeGoal Nothing
tagMadeGoal (Right x) = MadeGoal (Just $ madeGoal x)

tagArrivalRank :: Either String Tagging -> ArrivalRank
tagArrivalRank (Left _) = ArrivalRank Nothing
tagArrivalRank (Right x) = ArrivalRank (Just $ arrivalRank x)

madeGoal :: Tagging
         -> Pilot
         -> SpeedSection
         -> IxTask
         -> Kml.MarkedFixes
         -> Maybe Bool
madeGoal _ _ Nothing _ _ = Nothing
madeGoal x pilot speedSection (IxTask i) _ =
    case (tagging x) ^? element (fromIntegral i - 1) of
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
    case (timing x) ^? element (fromIntegral i - 1) of
        Nothing -> Nothing
        Just TrackTime{..} -> arrivalRankPilot pilot speedSection zonesRankPilot

arrivalRankPilot :: Pilot -> SpeedSection -> [[Pilot]] -> Maybe Int
arrivalRankPilot _ Nothing _ = Nothing
arrivalRankPilot p speedSection xss =
    case pss of
        [] -> Nothing
        (ps : _) -> elemIndex p ps
    where
        pss :: [[Pilot]] = reverse $ slice speedSection xss
