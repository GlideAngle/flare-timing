module Flight.LeadArea (readCompLeading, writeCompLeading) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Lead (CompLeading)
import Flight.Field (FieldOrdering(..))
import Flight.Comp (LeadAreaFile(..))

readCompLeading
    :: (MonadIO m, FromJSON (CompLeading q))
    => LeadAreaFile
    -> m (CompLeading q)
readCompLeading (LeadAreaFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompLeading
    :: (ToJSON (CompLeading q))
    => LeadAreaFile
    -> CompLeading q
    -> IO ()
writeCompLeading (LeadAreaFile path) leading = do
    let cfg = Y.setConfCompare (fieldOrder leading) Y.defConfig
    let yaml = Y.encodePretty cfg leading
    BS.writeFile path yaml
