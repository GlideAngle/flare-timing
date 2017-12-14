{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cmd.Inputs.TaskLength
    ( TaskLengthLookup(..)
    , readLengths
    , routeLength
    ) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Control.Lens ((^?), element)
import Flight.TaskTrack (TaskRoutes(..), TaskTrack(..), TrackLine(..))
import Flight.TrackLog (IxTask(..))
import Flight.TaskTrack.Double (fromKm)
import Flight.Task (TaskDistance(..))
import Flight.Comp (TaskLengthFile(..))

type RoutesLookup a = IxTask -> Maybe a

newtype TaskLengthLookup =
    TaskLengthLookup (Maybe (RoutesLookup (TaskDistance Double)))

readLengths :: TaskLengthFile -> ExceptT String IO TaskRoutes
readLengths (TaskLengthFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents

routeLength :: Either String TaskRoutes -> TaskLengthLookup
routeLength (Left _) = TaskLengthLookup Nothing
routeLength (Right x) = TaskLengthLookup (Just $ length x)

length :: TaskRoutes -> IxTask -> Maybe (TaskDistance Double)
length TaskRoutes{taskRoutes} (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    d <- edgeToEdge x
    return . fromKm . distance $ d
