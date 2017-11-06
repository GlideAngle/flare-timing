{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.LatLng.Raw (RawLatLng(..))

-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    | TaskDistanceByProjection
    deriving (Eq, Show)

newtype TaskRoutes =
    TaskRoutes { taskRoutes :: [Maybe TaskTrack] }
    deriving (Show, Generic)

instance ToJSON TaskRoutes
instance FromJSON TaskRoutes

data TaskTrack =
    TaskTrack { pointToPoint :: Maybe TrackLine
              , edgeToEdge :: Maybe TrackLine
              , projection :: Maybe ProjectedTrackLine
              }
    deriving (Show, Generic)

instance ToJSON TaskTrack
instance FromJSON TaskTrack

data ProjectedTrackLine =
    ProjectedTrackLine { planar :: PlanarTrackLine
                       , spherical :: TrackLine
                       }
    deriving (Show, Generic)

instance ToJSON ProjectedTrackLine
instance FromJSON ProjectedTrackLine

data TrackLine =
    TrackLine { distance :: Double
              , waypoints :: [RawLatLng]
              , legs :: [Double]
              , legsSum :: [Double]
              }
    deriving (Show, Generic)

instance ToJSON TrackLine
instance FromJSON TrackLine

data PlanarTrackLine =
    PlanarTrackLine { distance :: Double
                    , mappedZones :: [UtmZone]
                    , mappedPoints :: [EastingNorthing]
                    , legs :: [Double]
                    , legsSum :: [Double]
                    }
    deriving (Show, Generic)

instance ToJSON PlanarTrackLine
instance FromJSON PlanarTrackLine

data EastingNorthing =
    EastingNorthing { easting :: Double
                    , northing :: Double
                    }
    deriving (Show, Generic)

instance ToJSON EastingNorthing
instance FromJSON EastingNorthing

data UtmZone =
    UtmZone { latZone :: Char
            , lngZone :: Int
            }
    deriving (Show, Eq, Generic)

instance ToJSON UtmZone
instance FromJSON UtmZone
