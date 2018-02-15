{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Flight.Distance (TaskDistance(..), PathDistance(..), SpanLatLng, toKm)
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.TaskTrack.Internal (convertLatLng, legDistances, addTaskDistance)
import Flight.Earth.Sphere.PointToPoint.Double (distanceHaversine)

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [RawLatLng]
              , legs :: [Double]
              , legsSum :: [Double]
              }
              deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

class ToTrackLine a b where
    toTrackLine :: SpanLatLng a -> Bool -> b -> TrackLine

instance ToTrackLine Double (PathDistance Double) where
    toTrackLine span excludeWaypoints ed =
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

instance ToTrackLine Rational (PathDistance Rational) where
    toTrackLine span excludeWaypoints ed =
        TrackLine
            { distance = toKm d
            , waypoints = if excludeWaypoints then [] else xs
            , legs = toKm <$> ds 
            , legsSum = toKm <$> dsSum
            }
        where
            d :: TaskDistance Rational
            d = edgesSum ed

            -- NOTE: The graph of points created for determining the shortest
            -- path can have duplicate points, so the shortest path too can have
            -- duplicate points. Remove these duplicates.
            --
            -- I found that by decreasing defEps, the default epsilon, used for
            -- rational math from 1/10^9 to 1/10^12 these duplicates stopped
            -- occuring.
            vertices' :: [LatLng Rational [u| rad |]]
            vertices' = nub $ vertices ed

            xs :: [RawLatLng]
            xs = convertLatLng <$> vertices'

            ds :: [TaskDistance Rational]
            ds =
                legDistances
                    distancePointToPoint
                    span
                    (Point <$> vertices' :: [Zone Rational])

            dsSum :: [TaskDistance Rational]
            dsSum = scanl1 addTaskDistance ds
