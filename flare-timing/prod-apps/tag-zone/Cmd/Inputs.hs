module Cmd.Inputs (readCrossings) where

import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Track.Cross (Crossing)
import Flight.Comp (CrossFile(..))

readCrossings :: CrossFile -> ExceptT String IO Crossing
readCrossings (CrossFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents
