module Flight.FlyTime (readCompFlying , writeCompFlying) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Cross (CompFlying)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (FlyTimeFile(..))

readCompFlying :: (MonadThrow m, MonadIO m) => FlyTimeFile -> m CompFlying
readCompFlying (FlyTimeFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompFlying :: FlyTimeFile -> CompFlying -> IO ()
writeCompFlying (FlyTimeFile path) flyTime = do
    let cfg = Y.setConfCompare (fieldOrder flyTime) Y.defConfig
    let yaml = Y.encodePretty cfg flyTime
    BS.writeFile path yaml
