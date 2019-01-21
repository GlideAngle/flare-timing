{-# LANGUAGE DuplicateRecordFields #-}
{-|
Module      : Flight.Track.Time
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
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

import Prelude hiding (seq)
import Data.Maybe (fromMaybe, catMaybes, maybeToList, listToMaybe)
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
import Flight.Score
    ( LeadingAreaStep(..)
    , LeadingCoefficient(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcPoint(..)
    , LcSeq(..)
    , LcTrack
    , LengthOfSs(..)
    , TaskDeadline(..)
    , EssTime(..)
    , areaSteps
    , showSecs
    )
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp (SpeedSection)

-- | Seconds from first speed zone crossing irrespective of start time.
newtype LeadTick = LeadTick Double
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

instance Show LeadTick where
    show (LeadTick t) = showSecs . toRational $ t

-- | Seconds from first speed zone crossing made at or after the start time.
newtype RaceTick = RaceTick Double
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

instance Show RaceTick where
    show (RaceTick t) = showSecs . toRational $ t

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
        , lat :: RawLat
        -- ^ Latitude of the fix
        , lng :: RawLng
        -- ^ Longitude of the fix
        , distance :: Double
        -- ^ Distance to goal in km
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
        , area :: LeadingAreaStep
        -- ^ Leading coefficient area step.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
            g (LeadingAreaStep x) = f $ fromRational x

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
        ys = (\TickRow{area = LeadingAreaStep a} -> a) <$> xs
leadingSum (Just _) (Just (start, _)) xs =
    if null ys then Nothing else
    Just . LeadingCoefficient $ sum ys
    where
        ys =
            (\TickRow{area = LeadingAreaStep a} -> a)
            <$> filter (\TickRow{leg} -> leg >= start) xs

leadingArea
    :: (Int -> Leg)
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> [TickRow]

leadingArea _ _ _ _ [] = []

-- NOTE: Everyone has bombed and no one has lead out from the start.
leadingArea _ _ Nothing _ _ = []

leadingArea _ Nothing _ _ xs = xs

leadingArea
    toLeg
    (Just (TaskDistance td))
    close@(Just (LeadClose (EssTime tt))) arrival rows =
    [ r{area = step}
    | r <- rows
    | step <- seq
    ]
    ++ extraRow
    where
        MkQuantity d = convert td :: Quantity Double [u| km |]

        lastRow = listToMaybe . take 1 . reverse $ rows

        LcSeq{seq, extra} =
            areaSteps
                (TaskDeadline $ MkQuantity tt)
                (LengthOfSs . MkQuantity . toRational $ d)
                (toLcTrack toLeg close arrival rows)

        extraRow = maybeToList $ do
            lr <- lastRow
            e <- extra
            return $ lr{area = e}

toLcPoint :: (Int -> Leg) -> TickRow -> Maybe LcPoint
toLcPoint _ TickRow{tickLead = Nothing} = Nothing
toLcPoint toLeg TickRow{leg, tickLead = Just (LeadTick t), distance} =
    Just LcPoint
        { leg = toLeg leg
        , mark = TaskTime . MkQuantity . toRational $ t
        , togo = DistanceToEss . MkQuantity . toRational $ distance
        }

-- | The time of last arrival at goal, in seconds from first lead.
newtype LeadArrival = LeadArrival EssTime

instance Show LeadArrival where
    show (LeadArrival (EssTime t)) = show (fromRational t :: Double)

-- | The time the task closes, in seconds from first lead.
newtype LeadClose = LeadClose EssTime

instance Show LeadClose where
    show (LeadClose (EssTime t)) = show (fromRational t :: Double)

toLcTrack
    :: (Int -> Leg)
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> LcTrack
toLcTrack toLeg tr ta xs =
    x{seq = reverse seq}
    where
        x@LcSeq{seq} = toLcTrackRev toLeg tr ta $ reverse xs

toLcTrackRev
    :: (Int -> Leg)
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> [TickRow]
    -> LcTrack

-- NOTE: Everyone has bombed and no one has lead out from the start.
toLcTrackRev _ Nothing _ _ = LcSeq{seq = [], extra = Nothing}

toLcTrackRev _ _ _ [] = LcSeq{seq = [], extra = Nothing}

toLcTrackRev
    toLeg
    (Just (LeadClose close))
    Nothing
    xs@(TickRow{tickLead, distance} : _) =
        -- NOTE: Everyone has landed out.
        LcSeq
            { seq = catMaybes $ Just y : (toLcPoint toLeg <$> xs)
            , extra = Nothing
            }
    where
        t' =
            case tickLead of
              Nothing -> close
              (Just (LeadTick t)) -> EssTime $ toRational t

        y = landOutRow
                (min close t')
                (DistanceToEss . MkQuantity . toRational $ distance)

toLcTrackRev
    toLeg
    (Just (LeadClose close))
    (Just (LeadArrival arrive))
    xs@(TickRow{tickLead, distance} : _) =
        -- NOTE: If distance <= 0 then goal was made.
        if distance <= 0
            then LcSeq{seq = xs', extra = Nothing}
            else LcSeq{seq = xs', extra = Just y}
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs

        t' =
            case tickLead of
              Nothing -> arrive
              (Just (LeadTick t)) -> EssTime $ toRational t

        y = landOutRow
                (min close . max arrive $ t')
                (DistanceToEss . MkQuantity . toRational $ distance)

landOutRow :: EssTime -> DistanceToEss -> LcPoint
landOutRow (EssTime t) d =
    LcPoint
        { leg = LandoutLeg 0
        , mark = TaskTime $ MkQuantity t
        , togo = d
        }

taskToLeading :: QTaskDistance Double [u| m |] -> LeadingDistance
taskToLeading (TaskDistance d) =
    LeadingDistance (convert d :: Quantity Double [u| km |])

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{leg, tickLead, tickRace, distance} =
    TickRow
        { leg = leg
        , tickLead = tickLead
        , tickRace = tickRace
        , distance = distance
        , area = LeadingAreaStep 0
        }

discard
    :: (Int -> Leg)
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe LeadClose
    -> Maybe LeadArrival
    -> Vector TimeRow
    -> Vector TickRow
discard toLeg dRace close arrival xs =
    V.fromList
    . leadingArea toLeg dRace close arrival
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
    | d x <= d y = discardFurther (x : ys)
    | otherwise = x : discardFurther (y : ys)
    where
        d = distance :: (TickRow -> Double)
discardFurther ys = ys

