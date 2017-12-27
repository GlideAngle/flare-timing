{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.Lookup.Route (routeLength) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Route (TaskRoutes(..), TaskTrack(..), TrackLine(..))
import Flight.Distance (TaskDistance(..))
import Flight.Comp (IxTask(..), RouteLookup(..))

-- | Convert from double with kilometres implied to metres.
fromKm :: Double -> TaskDistance Double
fromKm dRawKm =
    TaskDistance dm
    where 
        dKm :: Quantity Double [u| km |]
        dKm = MkQuantity dRawKm

        dm = convert dKm :: Quantity Double [u| m |]

routeLength :: Either String TaskRoutes -> RouteLookup
routeLength = RouteLookup . either (const Nothing) (Just . length)

length :: TaskRoutes -> IxTask -> Maybe (TaskDistance Double)
length TaskRoutes{taskRoutes} (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    d <- edgeToEdge x
    return . fromKm . distance $ d
