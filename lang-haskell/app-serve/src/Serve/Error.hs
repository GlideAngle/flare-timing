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
import Servant (ServerError, errBody, err400)

import Flight.Comp (PilotId(..), IxTask(..))

errPilotTrackNotFound :: IxTask -> PilotId -> ServerError
errPilotTrackNotFound (IxTask ix) (PilotId p) =
    err400
        { errBody = LBS.pack
        $ printf "For task %d, the tracklog for pilot %s was not found" ix p
        }

errPilotNotFound :: PilotId -> ServerError
errPilotNotFound (PilotId p) =
    err400 {errBody = LBS.pack $ printf "Pilot %s not found" p}

errTaskBounds :: Int -> ServerError
errTaskBounds ii =
    err400 {errBody = LBS.pack $ printf "Out of bounds task %d" ii}

errTaskLengths :: ServerError
errTaskLengths =
    err400 {errBody = LBS.pack "I need the lengths of each task" }

errTaskPoints :: ServerError
errTaskPoints =
    err400 {errBody = LBS.pack "I need the points of each task" }

errAltPoints :: ServerError
errAltPoints =
    err400 {errBody = LBS.pack "I need the expected points of each task" }

errTaskStep :: String -> Int -> ServerError
errTaskStep step ii =
    err400
        { errBody = LBS.pack
        $ "I need to have access to data from "
        ++ step
        ++ " for task: #"
        ++ show ii
        }
