{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.Lookup.TaskLength
    ( TaskLengthLookup(..)
    , routeLength
    ) where

import Prelude hiding (length)
import Control.Monad (join)
import Control.Lens ((^?), element)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Route (TaskRoutes(..), TaskTrack(..), TrackLine(..))
import Flight.Distance (TaskDistance(..))
import Flight.Comp (IxTask(..))

type RoutesLookup a = IxTask -> Maybe a

newtype TaskLengthLookup =
    TaskLengthLookup (Maybe (RoutesLookup (TaskDistance Double)))

-- | Convert from double with kilometres implied to metres.
fromKm :: Double -> TaskDistance Double
fromKm dRawKm =
    TaskDistance dm
    where 
        dKm :: Quantity Double [u| km |]
        dKm = MkQuantity dRawKm

        dm = convert dKm :: Quantity Double [u| m |]

routeLength :: Either String TaskRoutes -> TaskLengthLookup
routeLength (Left _) = TaskLengthLookup Nothing
routeLength (Right x) = TaskLengthLookup (Just $ length x)

length :: TaskRoutes -> IxTask -> Maybe (TaskDistance Double)
length TaskRoutes{taskRoutes} (IxTask i) = do
    x <- join $ taskRoutes ^? element (fromIntegral i - 1)
    d <- edgeToEdge x
    return . fromKm . distance $ d
