module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Route (TaskRoute(..), TaskTrack(..), TrackLine(..))
import Flight.Distance (TaskDistance(..))
import Flight.Comp (IxTask(..), RoutesLookupTaskDistance(..))

-- | Convert from double with kilometres implied to metres.
fromKm :: Double -> TaskDistance Double
fromKm dRawKm =
    TaskDistance dm
    where 
        dKm :: Quantity Double [u| km |]
        dKm = MkQuantity dRawKm

        dm = convert dKm :: Quantity Double [u| m |]

routeLength :: Either a TaskRoute -> RoutesLookupTaskDistance
routeLength = RoutesLookupTaskDistance . either (const Nothing) (Just . length)

length :: TaskRoute -> IxTask -> Maybe (TaskDistance Double)
length TaskRoute{taskRoute} (IxTask i) = do
    x <- join $ taskRoute ^? element (fromIntegral i - 1)
    d <- sphericalEdgeToEdge x
    return . fromKm . distance $ d
