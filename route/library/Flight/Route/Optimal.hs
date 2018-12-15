module Flight.Route.Optimal
    ( OptimalRoute(..)
    , emptyOptimal
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data OptimalRoute a =
    OptimalRoute
        { taskRoute :: a
        -- ^ The optimal route for the task, including the prolog before the
        -- speed section and the epilog after the speed section.
        , taskRouteSpeedSubset :: a
        -- ^ Those legs of the optimal route for the task within the speed
        -- section alone.
        , speedRoute :: a
        -- ^ The optimal route for the zones in the speed section with no
        -- regard to the other zones.
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

emptyOptimal :: OptimalRoute (Maybe a)
emptyOptimal =
    OptimalRoute
        { taskRoute = Nothing
        , taskRouteSpeedSubset = Nothing
        , speedRoute = Nothing
        }
