module Flight.Yaml.TaskLength (readRoutes) where

import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Route (TaskRoutes(..))
import Flight.Comp (TaskLengthFile(..))

readRoutes :: TaskLengthFile -> ExceptT String IO TaskRoutes
readRoutes (TaskLengthFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents
