{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Route
    ( ToTrackLine(..)
    , TaskDistanceMeasure(..)
    , TaskRoute(..)
    , TaskTrack(..)
    , TrackLine(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.EastNorth (EastingNorthing(..), UtmZone(..))
import Flight.Field (FieldOrdering(..))
import Flight.Route.TrackLine (ToTrackLine(..), TrackLine(..))

-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    | TaskDistanceByProjection
    deriving (Eq, Ord, Show)

data TaskRoute =
    TaskRoute
        { taskRoute :: [Maybe TaskTrack]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TaskTrack =
    TaskTrack
        { sphericalPointToPoint :: Maybe TrackLine
        , ellipsoidPointToPoint :: Maybe TrackLine
        , sphericalEdgeToEdge :: Maybe TrackLine
        , ellipsoidEdgeToEdge :: Maybe TrackLine
        , projection :: Maybe ProjectedTrackLine
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data ProjectedTrackLine =
    ProjectedTrackLine
        { spherical :: TrackLine
        , ellipsoid :: TrackLine
        , planar :: PlanarTrackLine
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data PlanarTrackLine =
    PlanarTrackLine
        { distance :: Double
        , mappedZones :: [UtmZone]
        , mappedPoints :: [EastingNorthing]
        , legs :: [Double]
        , legsSum :: [Double]
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

instance FieldOrdering TaskRoute where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        -- EastingNorthing fields
        ("easting", _) -> LT
        ("northing", _) -> GT

        -- UtmZone fields
        ("latZone", _) -> LT
        ("lngZone", _) -> GT

        -- TaskTrack fields
        ("sphericalPointToPoint", _) -> LT

        ("ellipsoidPointToPoint", "sphericalPointToPoint") -> GT
        ("ellipsoidPointToPoint", _) -> LT

        ("sphericalEdgeToEdge", "sphericalPointToPoint") -> GT
        ("sphericalEdgeToEdge", "ellipsoidPointToPoint") -> GT
        ("sphericalEdgeToEdge", _) -> LT

        ("ellipsoidEdgeToEdge", "sphericalPointToPoint") -> GT
        ("ellipsoidEdgeToEdge", "ellipsoidPointToPoint") -> GT
        ("ellipsoidEdgeToEdge", "sphericalEdgeToEdge") -> GT
        ("ellipsoidEdgeToEdge", _) -> LT

        ("projection", _) -> GT

        -- ProjectedTrackLine fields
        ("spherical", _) -> LT

        ("ellipsoid", "spherical") -> GT
        ("ellipsoid", _) -> LT

        ("planar", _) -> GT

        -- RawLatLng fields
        ("lat", _) -> LT
        ("lng", _) -> GT

        -- PlanarTrackLine fields
        ("distance", _) -> LT
        ("legs", "distance") -> GT
        ("legs", _) -> LT
        ("legsSum", "distance") -> GT
        ("legsSum", "legs") -> GT
        ("legsSum", _) -> LT
        ("wayPoints", _) -> GT
        ("mappedPoints", "distance") -> GT
        ("mappedPoints", "legs") -> GT
        ("mappedPoints", "legsSum") -> GT
        ("mappedPoints", _) -> LT
        ("mappedZones", _) -> GT
        _ -> compare a b
