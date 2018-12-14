module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u)

import Flight.Route (TaskTrack(..), TrackLine(..))
import Flight.Distance (QTaskDistance)
import Flight.Comp (IxTask(..), RoutesLookupTaskDistance(..))

routeLength :: Either a [Maybe TaskTrack] -> RoutesLookupTaskDistance
routeLength = RoutesLookupTaskDistance . either (const Nothing) (Just . length)

length
    :: [Maybe TaskTrack]
    -> IxTask
    -> Maybe (QTaskDistance Double [u| m |])
length taskRoutes (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    d <- sphericalEdgeToEdge x
    return . distance $ d
