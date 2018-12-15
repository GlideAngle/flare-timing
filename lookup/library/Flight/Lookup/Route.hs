module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)

import Flight.Route (OptimalRoute(..), TaskTrack(..), TrackLine(..))
import Flight.Comp
    (IxTask(..), RoutesLookupTaskDistance(..), TaskRouteDistance(..))

routeLength
    :: (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -- ^ Gets the task optimal route to use.
    -> (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -- ^ Gets the speed section subset of the task optimal route to use.
    -> Either a [Maybe TaskTrack] -> RoutesLookupTaskDistance
routeLength f g =
    RoutesLookupTaskDistance
    . either (const Nothing) (Just . length f g)

length
    :: (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -> (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -> [Maybe TaskTrack]
    -> IxTask
    -> Maybe TaskRouteDistance
length f g taskRoutes (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    dTask <- f . sphericalEdgeToEdge $ x
    dSpeed <- g . sphericalEdgeToEdge $ x
    return
        TaskRouteDistance
            { wholeTaskDistance = distance dTask
            , speedSubsetDistance = distance dSpeed
            }
