{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Cmd.Outputs (writeTimeRowsToCsv) where

import Data.Csv
    ( EncodeOptions(..)
    , ToNamedRecord(..)
    , namedRecord
    , namedField
    , encodeByNameWith
    , defaultEncodeOptions
    )
import Data.List.Split (wordsBy)
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile, unpack)
import qualified Data.Vector as V (fromList)
import Data.HashMap.Strict (unions)
import Flight.Comp (Pilot(..))
import Flight.Track.Time (TimeRow(..))
import Data.Number.RoundingFunctions (dpRound)
import Data.Aeson (encode)

newtype Row = Row TimeRow

unquote :: String -> String
unquote s =
    case wordsBy (== '"') s of
        [x] -> x
        _ -> s

instance ToNamedRecord Row where
    toNamedRecord (Row TimeRow{..}) =
        unions [local, toNamedRecord lat, toNamedRecord lng]
        where
            local =
                namedRecord
                    [ namedField "time" time'
                    , namedField "pilot" p
                    , namedField "distance" $ show $ dpRound 8 (toRational distance)
                    ]


            time' = unquote . L.unpack . encode $ time
            Pilot p = pilot

writeTimeRowsToCsv :: FilePath -> [String] -> [TimeRow] -> IO ()
writeTimeRowsToCsv filename headers xs =
    L.writeFile filename rows
    where
        opts = defaultEncodeOptions {encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = encodeByNameWith opts hs $ Row <$> xs
