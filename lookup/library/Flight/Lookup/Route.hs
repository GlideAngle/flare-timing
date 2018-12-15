module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u)

import Flight.Route (OptimalRoute(..), TaskTrack(..), TrackLine(..))
import Flight.Distance (QTaskDistance)
import Flight.Comp (IxTask(..), RoutesLookupTaskDistance(..))

routeLength
    :: (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -> Either a [Maybe TaskTrack] -> RoutesLookupTaskDistance
routeLength f =
    RoutesLookupTaskDistance
    . either (const Nothing) (Just . length f)

length
    :: (OptimalRoute (Maybe TrackLine) -> Maybe TrackLine)
    -> [Maybe TaskTrack]
    -> IxTask
    -> Maybe (QTaskDistance Double [u| m |])
length f taskRoutes (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    d <- f . sphericalEdgeToEdge $ x
    return . distance $ d
