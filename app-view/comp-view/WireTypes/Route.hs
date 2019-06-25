{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Route
    ( TaskDistance(..)
    , TrackLine(..)
    , PlanarTrackLine(..)
    , OptimalRoute(..)
    , TaskLength(..)
    , TaskLegs(..)
    , TaskRoute(..)
    , TaskRouteSubset(..)
    , SpeedRoute(..)
    , stopTaskLength
    , taskLength
    , taskLegs
    , lineToRoute
    , optimalTaskRoute
    , optimalTaskRouteSubset
    , optimalSpeedRoute
    , showTaskDistance
    ) where

import Text.Printf (printf)
import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import qualified Data.Text as T (Text, pack, unpack)

import WireTypes.Zone (RawLatLng(..))

newtype TaskDistance = TaskDistance Double
    deriving (Eq, Ord, Show, Generic)

instance FromJSON TaskDistance where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : 'k' : ' ' : xs -> return . TaskDistance . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showTaskDistance :: TaskDistance -> T.Text
showTaskDistance (TaskDistance d) =
    T.pack . printf "%.3f km" $ d

data TrackLine =
    TrackLine
        { distance :: TaskDistance
        , waypoints :: [RawLatLng]
        , legs :: [TaskDistance]
        , legsSum :: [TaskDistance]
        , flipSum :: [TaskDistance]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data PlanarTrackLine =
    PlanarTrackLine
        { distance :: TaskDistance
        , legs :: [TaskDistance]
        , legsSum :: [TaskDistance]
        , flipSum :: [TaskDistance]
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data OptimalRoute a =
    OptimalRoute
        { taskRoute :: a
        , taskRouteSpeedSubset :: a
        , speedRoute :: a
        , stopRoute :: a
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data TaskLength =
    TaskLength
        { taskRoute :: TaskDistance
        , taskRouteSpeedSubset :: TaskDistance
        }

data TaskLegs =
    TaskLegs
        { legs :: [TaskDistance]
        , legsSum :: [TaskDistance]
        , flipSum :: [TaskDistance]
        }

taskLength :: OptimalRoute (Maybe TrackLine) -> Maybe TaskLength
taskLength OptimalRoute{..} =
    case (taskRoute, taskRouteSpeedSubset) of
        (Just TrackLine{distance = x}, Just TrackLine{distance = y}) ->
            Just $
                TaskLength
                    { taskRoute = x
                    , taskRouteSpeedSubset = y
                    }

        _ -> Nothing

taskLegs :: OptimalRoute (Maybe TrackLine) -> Maybe TaskLegs
taskLegs OptimalRoute{..} =
    (\TrackLine{..} ->
        TaskLegs
            { legs = legs
            , legsSum = legsSum
            , flipSum = flipSum
            })
    <$> taskRoute

stopTaskLength :: OptimalRoute (Maybe TrackLine) -> Maybe TaskDistance
stopTaskLength OptimalRoute{..} =
    (\TrackLine{..} -> distance) <$> stopRoute

newtype TaskRoute = TaskRoute [RawLatLng]
newtype TaskRouteSubset = TaskRouteSubset [RawLatLng]
newtype SpeedRoute = SpeedRoute [RawLatLng]

lineToRoute :: Maybe TrackLine -> TaskRoute
lineToRoute taskRoute = TaskRoute $ maybe [] waypoints taskRoute

optimalTaskRoute :: OptimalRoute (Maybe TrackLine) -> TaskRoute
optimalTaskRoute OptimalRoute{..} = TaskRoute $ maybe [] waypoints taskRoute

optimalTaskRouteSubset :: OptimalRoute (Maybe TrackLine) -> TaskRouteSubset
optimalTaskRouteSubset OptimalRoute{..} = TaskRouteSubset $ maybe [] waypoints taskRouteSpeedSubset

optimalSpeedRoute :: OptimalRoute (Maybe TrackLine) -> SpeedRoute
optimalSpeedRoute OptimalRoute{..} = SpeedRoute $ maybe [] waypoints speedRoute
