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
    , LeadTick(..)
    , RaceTick(..)
    , TimeRow(..)
    , TickRow(..)
    , LeadArrival(..)
    , LeadClose(..)
    , leadingArea
    , leadingSum
    , minLeading
    , taskToLeading
    , discard
    ) where

import Data.Maybe (fromMaybe, catMaybes)
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
import Flight.Comp (SpeedSection)

-- | Seconds from first speed zone crossing irrespective of start time.
newtype LeadTick = LeadTick Double
    deriving (Eq, Ord, Num, Show, Generic, ToJSON, FromJSON, ToField, FromField)

-- | Seconds from first speed zone crossing made at or after the start time.
newtype RaceTick = RaceTick Double
    deriving (Eq, Ord, Num, Show, Generic, ToJSON, FromJSON, ToField, FromField)

newtype LeadingDistance = LeadingDistance (Quantity Double [u| km |])

-- | A fix but indexed off the first crossing time.
data TimeRow =
    TimeRow
        { leg :: Int
        -- ^ Leg of the task.
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , tickRace :: Maybe RaceTick
        -- ^ Seconds from first start.
        , time :: UTCTime
        -- ^ Time of the fix
        , lat :: ViaScientific RawLat
        -- ^ Latitude of the fix
        , lng :: ViaScientific RawLng
        -- ^ Longitude of the fix
        , distance :: Double
        -- ^ Distance to goal in km
        }
        deriving (Eq, Ord, Show, Generic)

instance ToJSON TimeRow
instance FromJSON TimeRow

-- | A fix but indexed off the first crossing time.
data TickRow =
    TickRow
        { leg :: Int
        -- ^ Leg of the task
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , tickRace :: Maybe RaceTick
        -- ^ Seconds from first start.
        , distance :: Double
        -- ^ Distance to goal in km.
        , area :: ViaScientific LeadingAreaStep
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
                    , namedField "tickLead" tickLead
                    , namedField "tickRace" tickRace
                    , namedField "time" time'
                    , namedField "distance" d
                    ]


            time' = unquote . unpack . encode $ time
            d = unquote . unpack . encode $ distance

instance FromNamedRecord TimeRow where
    parseNamedRecord m =
        TimeRow <$>
        m .: "leg" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        t <*>
        m .: "lat" <*>
        m .: "lng" <*>
        m .: "distance"
        where
            t = parseTime <$> m .: "time"

instance ToNamedRecord TickRow where
    toNamedRecord TickRow{..} =
        namedRecord
            [ namedField "leg" leg
            , namedField "tickLead" tickLead
            , namedField "tickRace" tickRace
            , namedField "distance" (f distance)
            , namedField "area" (g area)
            ]
        where
            f = unquote . unpack . encode
            g (ViaScientific (LeadingAreaStep x)) = f $ fromRational x

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "leg" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "distance" <*>
        m .: "area"

minLeading :: [LeadingCoefficient] -> Maybe LeadingCoefficient
minLeading xs =
    if null xs then Nothing else Just $ minimum xs

-- TODO: The GAP guide says that the best distance for zeroth time is the
-- leading distance. Describe how this interacts with the distance to goal of
-- the first point on course.
leadingSum
    :: Maybe LeadingDistance
    -> SpeedSection
    -> [TickRow]
    -> Maybe LeadingCoefficient
leadingSum Nothing _ _ = Nothing
leadingSum _ _ [] = Nothing
leadingSum (Just _) Nothing xs =
    Just . LeadingCoefficient $ sum ys
    where
        ys = (\TickRow{area = ViaScientific (LeadingAreaStep a)} -> a) <$> xs
leadingSum (Just _) (Just (start, _)) xs =
    if null ys then Nothing else
    Just . LeadingCoefficient $ sum ys
    where
        ys =
            (\TickRow{area = ViaScientific (LeadingAreaStep a)} -> a)
            <$> filter (\TickRow{leg} -> leg >= start) xs

leadingArea
    :: Maybe (TaskDistance Double)
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> [TickRow]

leadingArea _ _ _ [] = []

-- NOTE: Everyone has bombed and no one has lead out from the start.
leadingArea _ Nothing _ _ = []

leadingArea Nothing _ _ xs = xs

leadingArea
    (Just (TaskDistance (MkQuantity d)))
    close@(Just (LeadClose (EssTime tt))) arrival rows =
    if length rows /= length xs then rows else xs
    where
        -- TODO: Calculate the task deadline.
        steps =
            areaSteps
                deadline
                (LengthOfSs $ toRational d)
                (toLcTrack close arrival rows)

        xs =
            [ r{area = ViaScientific step}
            | r <- rows
            | step <- steps
            ]

        deadline = TaskDeadline tt

toLcPoint :: TickRow -> Maybe (TaskTime, DistanceToEss)
toLcPoint TickRow{tickLead = Nothing} = Nothing
toLcPoint TickRow{tickLead = Just (LeadTick t), distance} =
    Just (TaskTime $ toRational t, DistanceToEss $ toRational distance)

-- | The time of last arrival at goal, in seconds from first lead.
newtype LeadArrival = LeadArrival EssTime

-- | The time the task closes, in seconds from first lead.
newtype LeadClose = LeadClose EssTime

toLcTrack
    :: Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> LcTrack
toLcTrack tr ta xs =
    LcTrack . reverse . toLcTrackRev tr ta $ reverse xs

toLcTrackRev
    :: Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> [(TaskTime, DistanceToEss)]

-- NOTE: Everyone has bombed and no one has lead out from the start.
toLcTrackRev Nothing _ _ = []

toLcTrackRev _ _ [] = []

toLcTrackRev
    (Just (LeadClose (EssTime tr')))
    Nothing
    xs@(TickRow{tickLead, distance} : _) =
        -- NOTE: Everyone has landed out.
        catMaybes $
        case tickLead of
            Nothing -> toLcPoint <$> xs
            (Just (LeadTick t)) -> (Just $ y t) : (toLcPoint <$> xs)
    where
        y lead = landOutRow
                (EssTime $ min tr' (toRational lead))
                (DistanceToEss $ toRational distance)

toLcTrackRev
    (Just (LeadClose (EssTime tr')))
    (Just (LeadArrival arrive@(EssTime ta')))
    xs@(TickRow{tickLead, distance} : _) =
        -- NOTE: If distance <= 0 then goal was made.
        catMaybes $
        if distance <= 0 then toLcPoint <$> xs else
        case tickLead of
            Nothing -> toLcPoint <$> xs
            (Just (LeadTick t)) -> (Just $ y t) : (toLcPoint <$> xs)
    where
        ta = fromRational ta'

        y lead = landOutRow
                (if lead < ta then arrive else EssTime $ min tr' (toRational lead))
                (DistanceToEss $ toRational distance)

landOutRow :: EssTime -> DistanceToEss -> (TaskTime, DistanceToEss)
landOutRow (EssTime t) d = (TaskTime t, d)

taskToLeading :: TaskDistance Double -> LeadingDistance
taskToLeading (TaskDistance d) =
    LeadingDistance $ d'
    where
        d' = convert d :: Quantity Double [u| km |]

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{leg, tickLead, tickRace, distance} =
    TickRow
        { leg = leg
        , tickLead = tickLead
        , tickRace = tickRace
        , distance = distance
        , area = ViaScientific (LeadingAreaStep 0)
        }

discard
    :: Maybe (TaskDistance Double)
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> Vector TimeRow
    -> Vector TickRow
discard dRace close arrival xs =
    V.fromList
    . leadingArea dRace close arrival
    . discardFurther
    . dropZeros
    . V.toList
    $ timeToTick <$> xs

-- | Drop any rows where the distance is zero.
dropZeros :: [TickRow] -> [TickRow]
dropZeros =
    dropWhile ((== 0) . d)
    where
        d = distance :: (TickRow -> Double)

-- | Discard fixes further from goal than any previous fix.
discardFurther :: [TickRow] -> [TickRow]
discardFurther (x : y : ys)
    | d x < d y = discardFurther (x : ys)
    | otherwise = x : discardFurther (y : ys)
    where
        d = distance :: (TickRow -> Double)
discardFurther ys = ys

