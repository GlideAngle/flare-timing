module Serve.Error
    ( errTaskPoints
    , errAltPoints
    , errTaskBounds
    , errTaskStep
    , errPilotTrackNotFound
    , errPilotNotFound
    , errTaskLengths
    ) where

import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import Servant (ServantErr, errBody, err400)

import Flight.Comp (PilotId(..), IxTask(..))

errPilotTrackNotFound :: IxTask -> PilotId -> ServantErr
errPilotTrackNotFound (IxTask ix) (PilotId p) =
    err400
        { errBody = LBS.pack
        $ printf "For task %d, the tracklog for pilot %s was not found" ix p
        }

errPilotNotFound :: PilotId -> ServantErr
errPilotNotFound (PilotId p) =
    err400 {errBody = LBS.pack $ printf "Pilot %s not found" p}

errTaskBounds :: Int -> ServantErr
errTaskBounds ii =
    err400 {errBody = LBS.pack $ printf "Out of bounds task %d" ii}

errTaskLengths :: ServantErr
errTaskLengths =
    err400 {errBody = LBS.pack "I need the lengths of each task" }

errTaskPoints :: ServantErr
errTaskPoints =
    err400 {errBody = LBS.pack "I need the points of each task" }

errAltPoints :: ServantErr
errAltPoints =
    err400 {errBody = LBS.pack "I need the expected points of each task" }

errTaskStep :: String -> Int -> ServantErr
errTaskStep step ii =
    err400
        { errBody = LBS.pack
        $ "I need to have access to data from "
        ++ step
        ++ " for task: #"
        ++ show ii
        }
