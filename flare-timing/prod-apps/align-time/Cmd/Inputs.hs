module Cmd.Inputs (readTags) where

import Control.Monad.Except (ExceptT(..), lift)
import qualified Data.ByteString as BS
import Data.Yaml (decodeEither)
import Flight.Track.Tag (Tagging)
import Flight.Comp (TagZoneFile(..))

readTags :: TagZoneFile -> ExceptT String IO Tagging
readTags (TagZoneFile path) = do
    contents <- lift $ BS.readFile path
    ExceptT . return $ decodeEither contents
