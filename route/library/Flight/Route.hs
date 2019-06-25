{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Route
    ( ToTrackLine(..)
    , TaskDistanceMeasure(..)
    , OptimalRoute(..)
    , TaskTrack(..)
    , TrackLine(..)
    , GeoLines(..)
    , ProjectedTrackLine(..)
    , PlanarTrackLine(..)
    , cmpFields
    ) where

import Data.String (IsString())
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Flight.Field (FieldOrdering(..))
import Flight.Route.TrackLine
    ( ToTrackLine(..), TrackLine(..), GeoLines(..)
    , ProjectedTrackLine(..), PlanarTrackLine(..)
    )
import Flight.Route.Optimal (OptimalRoute(..))

-- | The way to measure the task distance.
data TaskDistanceMeasure
    = TaskDistanceByAllMethods
    | TaskDistanceByPoints
    | TaskDistanceByEdges
    | TaskDistanceByProjection
    deriving (Eq, Ord, Show)

data TaskTrack =
    TaskTrack
        { sphericalPointToPoint :: OptimalRoute (Maybe TrackLine)
        , ellipsoidPointToPoint :: OptimalRoute (Maybe TrackLine)
        , sphericalEdgeToEdge :: OptimalRoute (Maybe TrackLine)
        , ellipsoidEdgeToEdge :: OptimalRoute (Maybe TrackLine)
        , projection :: OptimalRoute (Maybe ProjectedTrackLine)
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance FieldOrdering TaskTrack where
    fieldOrder _ = cmpFields

cmpFields :: (Ord a, IsString a) => a -> a -> Ordering
cmpFields a b =
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

        ("flipSum", "distance") -> GT
        ("flipSum", "legs") -> GT
        ("flipSum", "legsSum") -> GT
        ("flipSum", _) -> LT

        ("wayPoints", _) -> GT

        ("mappedPoints", "distance") -> GT
        ("mappedPoints", "legs") -> GT
        ("mappedPoints", "legsSum") -> GT
        ("mappedPoints", _) -> LT
        ("mappedZones", _) -> GT

        -- OptimalRoute fields
        ("taskRoute", _) -> LT
        ("taskRouteSpeedSubset", "taskRoute") -> LT
        ("taskRouteSpeedSubset", _) -> LT

        ("speedRoute", "stopRoute") -> LT
        ("speedRoute", _) -> GT

        ("stopRoute", _) -> GT

        _ -> compare a b
