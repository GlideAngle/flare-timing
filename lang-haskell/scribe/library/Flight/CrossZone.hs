module Flight.CrossZone (readCrossing, writeCrossing) where

import Prelude hiding (readFile, writeFile)
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Yaml (decodeThrow)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Yaml.Pretty as Y
import Data.UnitsOfMeasure (KnownUnit, Unpack)

import Flight.Track.Cross (CompCrossing)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (CrossZoneFile(..))

readCrossing :: (MonadThrow m, MonadIO m) => CrossZoneFile -> m CompCrossing
readCrossing (CrossZoneFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCrossing :: CrossZoneFile -> CompCrossing -> IO ()
writeCrossing (CrossZoneFile path) crossZone = do
    let cfg = Y.setConfCompare (fieldOrder crossZone) Y.defConfig
    let yaml = Y.encodePretty cfg crossZone
    BS.writeFile path yaml
