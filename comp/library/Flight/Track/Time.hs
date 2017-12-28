{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

{-|
Module      : Flight.Track.Time
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Track fixes indexed on when the first pilot starts the speed section.
-}
module Flight.Track.Time
    ( LeadingDistance(..)
    , RaceTick(..)
    , TimeRow(..)
    , TickRow(..)
    , TickTask(..)
    , TickRace(..)
    , discardFurther
    , leadingArea
    , leadingSum
    , minLeading
    , taskToLeading
    , discard
    ) where

import Data.Maybe (fromMaybe)
import Data.Csv
    ( ToNamedRecord(..), FromNamedRecord(..)
    , ToField(..), FromField(..)
    , (.:)
    , namedRecord, namedField
    )
import Data.List.Split (wordsBy)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.HashMap.Strict (unions)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)

import Flight.Units ()
import Flight.LatLng.Raw (RawLat, RawLng)
import Data.Aeson.ViaScientific (ViaScientific(..))
import Flight.Score
    ( LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , LcTrack(..)
    , LengthOfSs(..)
    , TaskDeadline(..)
    , areaSteps
    )
import Flight.Distance (TaskDistance(..))
import Flight.Score (EssTime(..))

-- | Seconds from first speed zone crossing
newtype RaceTick = RaceTick Double
    deriving (Eq, Ord, Num, Show, Generic, ToJSON, FromJSON, ToField, FromField)

newtype LeadingDistance = LeadingDistance (Quantity Double [u| km |])

-- | A fix but indexed off the first crossing time.
data TimeRow =
    TimeRow
        { leg :: Int
        -- ^ Leg of the task
        , time :: UTCTime
        -- ^ Time of the fix
        , lat :: ViaScientific RawLat
        -- ^ Latitude of the fix
        , lng :: ViaScientific RawLng
        -- ^ Longitude of the fix
        , tick :: RaceTick
        -- ^ Seconds from first speed zone crossing
        , distance :: Double
        -- ^ Distance to goal in km
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TimeRow
instance FromJSON TimeRow

-- | A fix but indexed off the first crossing time.
data TickRow =
    TickRow
        { tick :: RaceTick
        -- ^ Seconds from first speed zone crossing.
        , distance :: Double
        -- ^ Distance to goal in km.
        , areaStep :: ViaScientific LeadingAreaStep
        -- ^ Leading coefficient area step.
        }
        deriving (Eq, Ord, Show, Generic)

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
            , namedField "distance" (f distance)
            , namedField "areaStep" (g areaStep)
            ]
        where
            f = unquote . unpack . encode
            g (ViaScientific (LeadingAreaStep x)) = f $ fromRational x

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "tick" <*>
        m .: "distance" <*>
        m .: "areaStep"

-- | Discard fixes further from goal than any previous fix.
discardFurther :: [TickRow] -> [TickRow]
discardFurther (x : y : ys)
    | d x < d y = discardFurther (x : ys)
    | otherwise = x : discardFurther (y : ys)
    where
        d = distance :: (TickRow -> Double)
discardFurther ys = ys

minLeading :: [LeadingCoefficient] -> Maybe LeadingCoefficient
minLeading xs =
    if null xs then Nothing else Just $ minimum xs

leadingSum :: Maybe LeadingDistance -> [TickRow] -> LeadingCoefficient
leadingSum Nothing _ = LeadingCoefficient 0
leadingSum (Just _) xs =
    LeadingCoefficient $ sum ys
    where
        ys = (\TickRow{areaStep = ViaScientific (LeadingAreaStep a)} -> a) <$> xs

leadingArea
    :: Maybe (TaskDistance Double)
    -> TickTask
    -> TickRace
    -> [TickRow]
    -> [TickRow]
leadingArea _ _ _ [] = []
leadingArea _ _ _ [x] = [x]
leadingArea Nothing _ _ xs = xs
leadingArea
    dRace@(Just (TaskDistance (MkQuantity d)))
    tickT@(TickTask (EssTime tt)) tickR
    rows@(xRow@TickRow{tick = x} : yRow@TickRow{tick = y} : ys)
        | y <= 0 =
            xRow : yRow : leadingArea dRace tickT tickR ys
        | x <= 0 && y > 0 =
            xRow : leadingArea dRace tickT tickR (yRow : ys)
        | otherwise =
            if length rows /= length xs then rows else xs
            where
                -- TODO: Calculate the task deadline.
                steps =
                    areaSteps
                        deadline
                        (LengthOfSs $ toRational d)
                        (toLcTrack tickT tickR rows)

                xs =
                    [ r{areaStep = ViaScientific step}
                    | r <- rows
                    | step <- steps
                    ]

                -- dRace = taskToLeading <$> taskLength
                deadline = TaskDeadline tt

toLcPoint :: TickRow -> (TaskTime, DistanceToEss)
toLcPoint TickRow{tick = RaceTick t, distance} =
    (TaskTime $ toRational t, DistanceToEss $ toRational distance)

newtype TickTask = TickTask EssTime
newtype TickRace = TickRace EssTime

toLcTrack :: TickTask -> TickRace -> [TickRow] -> LcTrack
toLcTrack (TickTask et) (TickRace er@(EssTime tr')) xs =
    case reverse xs of
        [] -> LcTrack []
        (TickRow{tick = RaceTick t, distance} : _) ->
            LcTrack $ (toLcPoint <$> xs) ++ ys
            where
                ys =
                    if distance <= 0 then [] else
                    landOutRows
                        (if t < tr then er else et)
                        (DistanceToEss $ toRational distance)
    where
        tr = fromRational tr'

landOutRows :: EssTime -> DistanceToEss -> [(TaskTime, DistanceToEss)]
landOutRows (EssTime t) d = [(TaskTime t, d)]

taskToLeading :: TaskDistance Double -> LeadingDistance
taskToLeading (TaskDistance d) =
    LeadingDistance $ d'
    where
        d' = convert d :: Quantity Double [u| km |]

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{tick, distance} =
    TickRow tick distance $ ViaScientific (LeadingAreaStep 0)

discard
    :: Maybe (TaskDistance Double)
    -> TickTask
    -> TickRace
    -> Vector TimeRow
    -> Vector TickRow
discard deadline tickR dRace xs =
    V.fromList
    . leadingArea deadline tickR dRace
    . discardFurther
    . dropZeros
    . V.toList
    $ timeToTick <$> xs

dropZeros :: [TickRow] -> [TickRow]
dropZeros =
    dropWhile ((== 0) . d)
    where
        d = distance :: (TickRow -> Double)
