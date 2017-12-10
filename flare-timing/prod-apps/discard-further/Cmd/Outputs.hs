module Cmd.Outputs (writeTimeRowsToCsv) where

import Data.Csv (EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import qualified Data.Vector as V (fromList, toList)
import Data.Vector (Vector)
import Flight.Track.Time (TickRow(..))
import Flight.Comp (DiscardFile(..))

writeTimeRowsToCsv :: DiscardFile -> [String] -> Vector TickRow -> IO ()
writeTimeRowsToCsv (DiscardFile path) headers xs =
    L.writeFile path rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs $ V.toList xs
