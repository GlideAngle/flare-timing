{-# LANGUAGE ScopedTypeVariables #-}

module Cmd.Inputs (readTimeRowsFromCsv) where

import Control.Monad.Except (ExceptT(..), lift)
import System.FilePath (FilePath)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (Header, decodeByName)
import Data.Vector (Vector)
import Flight.Track.Time (TimeRow(..))

readTimeRowsFromCsv :: FilePath
                    -> ExceptT String IO (Header, Vector TimeRow)
readTimeRowsFromCsv csvPath = do
    contents <- lift $ BL.readFile csvPath
    ExceptT . return $ decodeByName contents
