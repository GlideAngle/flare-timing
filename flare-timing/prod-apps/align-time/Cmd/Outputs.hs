module Cmd.Outputs (writeTimeRowsToCsv) where

import Data.Csv (EncodeOptions(..), encodeByNameWith, defaultEncodeOptions)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import qualified Data.Vector as V (fromList)
import Flight.Track.Time (TimeRow(..))

writeTimeRowsToCsv :: FilePath -> [String] -> [TimeRow] -> IO ()
writeTimeRowsToCsv filename headers xs =
    L.writeFile filename rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs xs
