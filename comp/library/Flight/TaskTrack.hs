{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Flight.TaskTrack
    ( TaskDistanceMeasure(..)
    , TaskRoutes(..)
    , TaskTrack(..)
    , TrackLine(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    , EastingNorthing(..)
    , UtmZone(..)
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.LatLng.Raw (RawLatLng(..))
import Flight.Comp (FieldOrdering(..))

-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    | TaskDistanceByProjection
    deriving (Eq, Ord, Show)

newtype TaskRoutes =
    TaskRoutes { taskRoutes :: [Maybe TaskTrack] }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON TaskRoutes
instance FromJSON TaskRoutes

data TaskTrack =
    TaskTrack { pointToPoint :: Maybe TrackLine
              , edgeToEdge :: Maybe TrackLine
              , projection :: Maybe ProjectedTrackLine
              }
              deriving (Eq, Ord, Show, Generic)

instance ToJSON TaskTrack
instance FromJSON TaskTrack

data ProjectedTrackLine =
    ProjectedTrackLine { planar :: PlanarTrackLine
                       , spherical :: TrackLine
                       }
                       deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectedTrackLine
instance FromJSON ProjectedTrackLine

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [RawLatLng]
              , legs :: [Double]
              , legsSum :: [Double]
              }
              deriving (Eq, Ord, Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

data PlanarTrackLine =
    PlanarTrackLine { distance :: Double
                    , mappedZones :: [UtmZone]
                    , mappedPoints :: [EastingNorthing]
                    , legs :: [Double]
                    , legsSum :: [Double]
                    }
                    deriving (Eq, Ord, Show, Generic)

instance ToJSON PlanarTrackLine
instance FromJSON PlanarTrackLine

data EastingNorthing =
    EastingNorthing { easting :: Double
                    , northing :: Double
                    }
                    deriving (Eq, Ord, Show, Generic)

instance ToJSON EastingNorthing
instance FromJSON EastingNorthing

data UtmZone =
    UtmZone { latZone :: Char
            , lngZone :: Int
            }
            deriving (Eq, Ord, Show, Generic)

instance ToJSON UtmZone
instance FromJSON UtmZone

instance FieldOrdering TaskRoutes where
    fieldOrder _ = cmp

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    case (a, b) of
        ("easting", _) -> LT
        ("northing", _) -> GT
        ("latZone", _) -> LT
        ("lngZone", _) -> GT
        ("pointToPoint", _) -> LT
        ("projection", "pointToPoint") -> GT
        ("projection", _) -> LT
        ("edgeToEdge", _) -> GT
        ("lat", _) -> LT
        ("lng", _) -> GT
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

