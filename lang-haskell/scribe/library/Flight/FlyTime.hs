module Flight.FlyTime (readFlying , writeFlying) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Cross (Flying)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (FlyTimeFile(..))

readFlying :: (MonadThrow m, MonadIO m) => FlyTimeFile -> m Flying
readFlying (FlyTimeFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeFlying :: FlyTimeFile -> Flying -> IO ()
writeFlying (FlyTimeFile path) flyTime = do
    let cfg = Y.setConfCompare (fieldOrder flyTime) Y.defConfig
    let yaml = Y.encodePretty cfg flyTime
    BS.writeFile path yaml
