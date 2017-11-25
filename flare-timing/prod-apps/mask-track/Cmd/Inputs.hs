{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cmd.Inputs (MadeGoal(..), readTags, taggedGoal) where

import Data.List (find)
import Control.Monad (join)
import Control.Monad.Except (ExceptT(..), lift)
import Control.Lens ((^?), element)
import System.FilePath (FilePath)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.TrackLog (IxTask(..))
import qualified Flight.Kml as Kml (MarkedFixes(..))
import Flight.Comp (SpeedSection, Pilot(..))
import Flight.Track.Tag (Tagging(..), TrackTag(..), PilotTrackTag(..))
import Flight.Mask (slice)

newtype MadeGoal =
    MadeGoal (Maybe (Pilot -> SpeedSection -> IxTask -> Kml.MarkedFixes -> Maybe Bool))

readTags :: FilePath -> ExceptT String IO Tagging
readTags yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    ExceptT . return $ decodeEither contents

taggedGoal :: Either String Tagging -> MadeGoal
taggedGoal (Left _) = MadeGoal Nothing
taggedGoal (Right x) = MadeGoal (Just $ madeGoal x)

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
