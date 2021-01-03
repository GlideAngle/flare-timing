module Flight.Mask.Reach
    ( readCompMaskReach, writeCompMaskReach
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Mask (CompMaskingReach(..))
import Flight.Field (FieldOrdering(..))
import Flight.Comp (MaskReachFile(..))

readCompMaskReach :: MonadIO m => MaskReachFile -> m CompMaskingReach
readCompMaskReach (MaskReachFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompMaskReach :: MaskReachFile -> CompMaskingReach -> IO ()
writeCompMaskReach (MaskReachFile path) maskReach = do
    let cfg = Y.setConfCompare (fieldOrder maskReach) Y.defConfig
    let yaml = Y.encodePretty cfg maskReach
    BS.writeFile path yaml
