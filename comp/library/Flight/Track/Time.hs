{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Flight.Track.Time
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Track fixes indexed on when the first pilot starts the speed section.
-}
module Flight.Track.Time (TimeRow(..), TickRow(..)) where

import Data.Maybe (fromMaybe)
import Data.Csv
    (ToNamedRecord(..), FromNamedRecord(..), (.:), namedRecord, namedField)
import Data.List.Split (wordsBy)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.HashMap.Strict (unions)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Flight.LatLng.Raw (RawLat, RawLng)

-- | A fix but indexed off the first crossing time.
data TimeRow =
    TimeRow
        { leg :: Int -- ^ Leg of the task
        , time :: UTCTime -- ^ Time of the fix
        , lat :: RawLat -- ^ Latitude of the fix
        , lng :: RawLng -- ^ Longitude of the fix
        , tick :: Double -- ^ Seconds from first speed zone crossing
        , distance :: Double -- ^ Distance to goal in km
        }
    deriving (Show, Generic)

instance ToJSON TimeRow
instance FromJSON TimeRow

-- | A fix but indexed off the first crossing time.
data TickRow =
    TickRow
        { tick :: Double -- ^ Seconds from first speed zone crossing
        , distance :: Double -- ^ Distance to goal in km
        }
    deriving (Show, Generic)

instance ToJSON TickRow
instance FromJSON TickRow

quote :: String -> String
quote s = "\"" ++ s ++ "\""

unquote :: String -> String
unquote s =
    case wordsBy (== '"') s of
        [x] -> x
        _ -> s

parseTime :: Maybe String -> UTCTime
parseTime Nothing = read ""
parseTime (Just s) = fromMaybe (read "") $ decode . pack . quote $ s

instance ToNamedRecord TimeRow where
    toNamedRecord TimeRow{..} =
        unions [local, toNamedRecord lat, toNamedRecord lng]
        where
            local =
                namedRecord
                    [ namedField "leg" leg
                    , namedField "time" time'
                    , namedField "tick" tick
                    , namedField "distance" d
                    ]


            time' = unquote . unpack . encode $ time
            d = unquote . unpack . encode $ distance

instance FromNamedRecord TimeRow where
    parseNamedRecord m =
        TimeRow <$>
        m .: "leg" <*>
        t <*>
        m .: "lat" <*>
        m .: "lng" <*>
        m .: "tick" <*>
        m .: "distance"
        where
            t = parseTime <$> m .: "time"

instance ToNamedRecord TickRow where
    toNamedRecord TickRow{..} =
        namedRecord
            [ namedField "tick" tick
            , namedField "distance" d
            ]
        where
            d = unquote . unpack . encode $ distance

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "tick" <*>
        m .: "distance"
