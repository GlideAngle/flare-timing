module Flight.PegFrame (readCompPegFrame, writeCompPegFrame) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Stop (CompFraming(..))
import Flight.Field (FieldOrdering(..))
import Flight.Comp (PegFrameFile(..))

readCompPegFrame :: (MonadThrow m, MonadIO m) => PegFrameFile -> m CompFraming
readCompPegFrame (PegFrameFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompPegFrame :: PegFrameFile -> CompFraming -> IO ()
writeCompPegFrame (PegFrameFile path) stopTask = do
    let cfg = Y.setConfCompare (fieldOrder stopTask) Y.defConfig
    let yaml = Y.encodePretty cfg stopTask
    BS.writeFile path yaml
