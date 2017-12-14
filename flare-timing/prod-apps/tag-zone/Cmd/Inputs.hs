module Cmd.Inputs (readCrossings) where

import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Track.Cross (Crossing)
import Flight.Comp (CrossZoneFile(..))

readCrossings :: CrossZoneFile -> ExceptT String IO Crossing
readCrossings (CrossZoneFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents
