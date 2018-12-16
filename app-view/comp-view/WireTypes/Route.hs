{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Route where

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
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data OptimalRoute a =
    OptimalRoute
        { taskRoute :: a
        , taskRouteSpeedSubset :: a
        , speedRoute :: a
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

data TaskLength =
    TaskLength
        { taskRoute :: TaskDistance
        , taskRouteSpeedSubset :: TaskDistance
        }

taskLength :: OptimalRoute (Maybe TrackLine) -> Maybe TaskLength
taskLength OptimalRoute{..} =
    case (taskRoute, taskRouteSpeedSubset) of
        (Just x, Just y) ->
            Just $
                TaskLength
                    { taskRoute = distance x
                    , taskRouteSpeedSubset = distance y
                    }

        _ -> Nothing
