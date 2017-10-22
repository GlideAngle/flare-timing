{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Cmd.Outputs (writeTimeRowsToCsv) where

import qualified Data.Csv as Csv
    ( EncodeOptions(..)
    , ToNamedRecord(..)
    , namedRecord
    , namedField
    , encodeByNameWith
    , defaultEncodeOptions
    )
import qualified Data.ByteString.Char8 as S (pack)
import qualified Data.ByteString.Lazy.Char8 as L (writeFile)
import qualified Data.Vector as V (fromList)
import Flight.Comp (Pilot(..))
import Flight.Track.Time (TimeRow(..))
import Flight.LatLng.Raw
import Data.Number.RoundingFunctions (dpRound)

newtype Row = Row TimeRow

instance Csv.ToNamedRecord Row where
    toNamedRecord (Row TimeRow{..}) =
        Csv.namedRecord
            [ Csv.namedField "time" $ show time
            , Csv.namedField "pilot" p
            , Csv.namedField "lat" $ show $ dpRound 8 lat'
            , Csv.namedField "lng" $ show $ dpRound 8 lng'
            , Csv.namedField "distance" $ show $ dpRound 8 (toRational distance)
            ]
        where
            RawLat lat' = lat
            RawLng lng' = lng
            Pilot p = pilot

writeTimeRowsToCsv :: FilePath -> [String] -> [TimeRow] -> IO ()
writeTimeRowsToCsv filename headers xs =
    L.writeFile filename rows
    where
        opts = Csv.defaultEncodeOptions {Csv.encUseCrLf = False}
        hs = V.fromList $ S.pack <$> headers
        rows = Csv.encodeByNameWith opts hs $ Row <$> xs
