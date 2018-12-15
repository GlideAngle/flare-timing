module Flight.Route.Optimal
    ( OptimalRoute(..)
    , emptyOptimal
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data OptimalRoute a =
    OptimalRoute
        { taskRoute :: a
        , ssRoute :: a
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

emptyOptimal :: OptimalRoute (Maybe a)
emptyOptimal = OptimalRoute { taskRoute = Nothing, ssRoute = Nothing }
