module Flight.Mask.Arrival
    ( readCompMaskArrival, writeCompMaskArrival
    , readAltArrival, writeAltArrival
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Mask (CompMaskingArrival)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (MaskArrivalFile(..), AltArrivalFile(..))

readAltArrival :: (MonadThrow m, MonadIO m) => AltArrivalFile -> m CompMaskingArrival
readAltArrival (AltArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeAltArrival :: AltArrivalFile -> CompMaskingArrival -> IO ()
writeAltArrival (AltArrivalFile path) track = do
    let cfg = Y.setConfCompare (fieldOrder track) Y.defConfig
    let yaml = Y.encodePretty cfg track
    BS.writeFile path yaml

readCompMaskArrival :: MonadIO m => MaskArrivalFile -> m CompMaskingArrival
readCompMaskArrival (MaskArrivalFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompMaskArrival :: MaskArrivalFile -> CompMaskingArrival -> IO ()
writeCompMaskArrival (MaskArrivalFile path) maskTrack = do
    let cfg = Y.setConfCompare (fieldOrder maskTrack) Y.defConfig
    let yaml = Y.encodePretty cfg maskTrack
    BS.writeFile path yaml
