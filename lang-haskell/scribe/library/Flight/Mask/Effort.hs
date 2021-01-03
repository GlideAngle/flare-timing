module Flight.Mask.Effort
    ( readCompMaskEffort, writeCompMaskEffort
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Mask (CompMaskingEffort)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (MaskEffortFile(..))

readCompMaskEffort :: MonadIO m => MaskEffortFile -> m CompMaskingEffort
readCompMaskEffort (MaskEffortFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompMaskEffort :: MaskEffortFile -> CompMaskingEffort -> IO ()
writeCompMaskEffort (MaskEffortFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml
