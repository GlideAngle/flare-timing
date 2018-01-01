{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Flight.Route.TrackLine
    ( ToTrackLine(..)
    , TrackLine(..)
    ) where

import Prelude hiding (span)
import Data.List (nub)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.UnitsOfMeasure (u)

import Flight.LatLng (LatLng(..))
import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone (Zone(..))
import Flight.Distance (toKm)
import Flight.TaskTrack.Internal (convertLatLng, legDistances, addTaskDistance)
import Flight.PointToPoint.Double (distancePointToPoint, distanceHaversine)
import Flight.Task (SpanLatLng)

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [RawLatLng]
              , legs :: [Double]
              , legsSum :: [Double]
              }
              deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

class ToTrackLine a where
    toTrackLine :: Bool -> a -> TrackLine

instance ToTrackLine (PathDistance Double) where
    toTrackLine = goByEdge

goByEdge :: Bool -> PathDistance Double -> TrackLine
goByEdge excludeWaypoints ed =
    TrackLine
        { distance = toKm d
        , waypoints = if excludeWaypoints then [] else xs
        , legs = toKm <$> ds 
        , legsSum = toKm <$> dsSum
        }
    where
        d :: TaskDistance Double
        d = edgesSum ed

        -- NOTE: The graph of points created for determining the shortest
        -- path can have duplicate points, so the shortest path too can have
        -- duplicate points. Remove these duplicates.
        --
        -- I found that by decreasing defEps, the default epsilon, used for
        -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
        -- occuring.
        vertices' :: [LatLng Double [u| rad |]]
        vertices' = nub $ vertices ed

        xs :: [RawLatLng]
        xs = convertLatLng <$> vertices'

        ds :: [TaskDistance Double]
        ds =
            legDistances
                distancePointToPoint
                span
                (Point <$> vertices' :: [Zone Double])

        dsSum :: [TaskDistance Double]
        dsSum = scanl1 addTaskDistance ds

span :: SpanLatLng Double
span = distanceHaversine
