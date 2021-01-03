module Flight.Mask.Bonus
    ( readCompMaskBonus, writeCompMaskBonus
    ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Except (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Yaml (decodeThrow)
import qualified Data.Yaml.Pretty as Y

import Flight.Track.Mask (CompMaskingReach(..))
import Flight.Field (FieldOrdering(..))
import Flight.Comp (BonusReachFile(..))

readCompMaskBonus :: MonadIO m => BonusReachFile -> m CompMaskingReach
readCompMaskBonus (BonusReachFile path) = liftIO $ BS.readFile path >>= decodeThrow

writeCompMaskBonus :: BonusReachFile -> CompMaskingReach -> IO ()
writeCompMaskBonus (BonusReachFile path) bonusReach = do
    let cfg = Y.setConfCompare (fieldOrder bonusReach) Y.defConfig
    let yaml = Y.encodePretty cfg bonusReach
    BS.writeFile path yaml
