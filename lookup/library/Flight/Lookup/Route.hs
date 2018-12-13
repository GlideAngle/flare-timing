module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u)

import Flight.Route (TaskRoute(..), TaskTrack(..), TrackLine(..))
import Flight.Distance (QTaskDistance)
import Flight.Comp (IxTask(..), RoutesLookupTaskDistance(..))

routeLength :: Either a TaskRoute -> RoutesLookupTaskDistance
routeLength = RoutesLookupTaskDistance . either (const Nothing) (Just . length)

length :: TaskRoute -> IxTask -> Maybe (QTaskDistance Double [u| m |])
length TaskRoute{taskRoute} (IxTask i) = do
    x <- join $ taskRoute ^? element (fromIntegral i - 1)
    d <- sphericalEdgeToEdge x
    return . distance $ d
