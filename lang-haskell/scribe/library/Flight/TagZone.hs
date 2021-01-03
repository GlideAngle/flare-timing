module Flight.TagZone (readTagging, writeTagging) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Tag (CompTagging(..))
import Flight.Field (FieldOrdering(..))
import Flight.Comp (TagZoneFile(..))

readTagging :: (MonadThrow m, MonadIO m) => TagZoneFile -> m CompTagging
readTagging (TagZoneFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeTagging :: TagZoneFile -> CompTagging -> IO ()
writeTagging (TagZoneFile path) tagZone = do
    let cfg = Y.setConfCompare (fieldOrder tagZone) Y.defConfig
    let yaml = Y.encodePretty cfg tagZone
    BS.writeFile path yaml
