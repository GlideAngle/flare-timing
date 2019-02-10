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
    ( AwardedVelocity(..)
    , LeadingDistance(..)
    , LeadTick(..)
    , RaceTick(..)
    , TrackRow(..)
    , TimeRow(..)
    , TickRow(..)
    , LeadAllDown(..)
    , LeadArrival(..)
    , LeadClose(..)
    , FixIdx(..)
    , ZoneIdx(..)
    , LegIdx(..)
    , leadingArea
    , leadingSum
    , minLeading
    , taskToLeading
    , discard
    , allHeaders
    , commentOnFixRange
    ) where

import Prelude hiding (seq)
import Data.Maybe (fromMaybe, catMaybes, maybeToList, listToMaybe)
import Data.Csv
    ( ToNamedRecord(..), FromNamedRecord(..)
    , ToField(..), FromField(..)
    , (.:)
    , namedRecord, namedField
    )
import Data.List ((\\), findIndices)
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
import Flight.Clip (FlyCut(..), FlyClipping(..))
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
    , Pilot
    , areaSteps
    , showSecs
    )
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Track.Range (asRanges)

data AwardedVelocity =
    AwardedVelocity
        { ss :: Maybe UTCTime
        , es :: Maybe UTCTime
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Show FixIdx where show (FixIdx x) = show x
instance Show ZoneIdx where show (ZoneIdx x) = show x
instance Show LegIdx where show (LegIdx x) = show x

-- | The index of a fix within the whole track.
newtype FixIdx = FixIdx Int
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Enum, Num, ToField, FromField)

-- | The index of a fix for a leg, that section of the tracklog between one zone and the next.
newtype ZoneIdx = ZoneIdx Int
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

-- | The index of a leg.
newtype LegIdx = LegIdx Int
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

-- | Seconds from first speed zone crossing irrespective of start time.
newtype LeadTick = LeadTick Double
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

instance Show LeadTick where
    show (LeadTick t) = showSecs $ toRational t

-- | Seconds from first speed zone crossing made at or after the start time.
newtype RaceTick = RaceTick Double
    deriving (Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Num, ToField, FromField)

instance Show RaceTick where
    show (RaceTick t) = showSecs $ toRational t

newtype LeadingDistance = LeadingDistance (Quantity Double [u| km |])

-- WARNING: I suspect cassava doesn't support repeated column names.

-- | To enable easy CSV comparison, each CSV file has the same headers but for
-- some files these columns are empty of data. The file comparisons I'm
-- interested in are unpack-track with align-time and align-time with
-- discard-further.
allHeaders :: [String]
allHeaders =
    ["fixIdx", "time", "lat", "lng"]
    ++ ["tickLead", "tickRace", "zoneIdx", "legIdx", "distance"]
    ++ ["area"]

data TrackRow =
    TrackRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , time :: UTCTime
        -- ^ Time of the fix
        , lat :: RawLat
        -- ^ Latitude of the fix
        , lng :: RawLng
        -- ^ Longitude of the fix
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A fix but with time as elapsed time from the first crossing time.
data TimeRow =
    TimeRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , time :: UTCTime
        -- ^ Time of the fix
        , lat :: RawLat
        -- ^ Latitude of the fix
        , lng :: RawLng
        -- ^ Longitude of the fix
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , tickRace :: Maybe RaceTick
        -- ^ Seconds from first start.
        , zoneIdx :: ZoneIdx
        -- ^ The fix number for this leg.
        , legIdx :: LegIdx
        -- ^ Leg of the task.
        , distance :: Double
        -- ^ Distance to goal in km
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

betweenTimeRow :: UTCTime -> UTCTime -> TimeRow -> Bool
betweenTimeRow t0 t1 TimeRow{time = t} =
    t0 <= t && t <= t1 

instance FlyClipping UTCTime [TimeRow] where
    clipToFlown x@FlyCut{cut = Nothing} =
        x{uncut = []}

    clipToFlown x@FlyCut{cut = Just (t0, t1), uncut = xs} =
        x{uncut = filter (betweenTimeRow t0 t1) xs}

    clipIndices FlyCut{cut = Nothing} = []

    clipIndices FlyCut{cut = Just (t0, t1), uncut = xs} =
        findIndices (betweenTimeRow t0 t1) xs

-- | A fix but with time as elapsed time from the first crossing time.
data TickRow =
    TickRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , tickRace :: Maybe RaceTick
        -- ^ Seconds from first start.
        , zoneIdx :: ZoneIdx
        -- ^ The fix number for this leg.
        , legIdx :: LegIdx
        -- ^ Leg of the task
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

instance ToNamedRecord TrackRow where
    toNamedRecord TrackRow{..} =
        unions [local, toNamedRecord lat, toNamedRecord lng, dummyTime, dummyTick]
        where
            local =
                namedRecord
                    [ namedField "fixIdx" fixIdx
                    , namedField "time" time'
                    ]

            -- NOTE: Fields in TimeRow not in TrackRow.
            dummyTime =
                namedRecord
                    [ namedField "tickLead" ("" :: String)
                    , namedField "tickRace" ("" :: String)
                    , namedField "zoneIdx" ("" :: String)
                    , namedField "legIdx" ("" :: String)
                    , namedField "distance" ("" :: String)
                    ]

            -- NOTE: Fields in TickRow that are not in TrackRow.
            dummyTick =
                namedRecord
                    [ namedField "area" ("" :: String)
                    ]

            time' = unquote . unpack . encode $ time

instance FromNamedRecord TrackRow where
    parseNamedRecord m =
        TrackRow <$>
        m .: "fixIdx" <*>
        t <*>
        m .: "lat" <*>
        m .: "lng"
        where
            t = parseTime <$> m .: "time"

instance ToNamedRecord TimeRow where
    toNamedRecord TimeRow{..} =
        unions [local, toNamedRecord lat, toNamedRecord lng, dummyTick]
        where
            local =
                namedRecord
                    [ namedField "fixIdx" fixIdx
                    , namedField "tickLead" tickLead
                    , namedField "tickRace" tickRace
                    , namedField "time" time'
                    , namedField "zoneIdx" zoneIdx
                    , namedField "legIdx" legIdx
                    , namedField "distance" d
                    ]

            -- NOTE: Fields in TickRow that are not in TimeRow.
            dummyTick =
                namedRecord
                    [ namedField "area" ("" :: String)
                    ]

            time' = unquote . unpack . encode $ time
            d = unquote . unpack . encode $ distance

instance FromNamedRecord TimeRow where
    parseNamedRecord m =
        TimeRow <$>
        m .: "fixIdx" <*>
        t <*>
        m .: "lat" <*>
        m .: "lng" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "zoneIdx" <*>
        m .: "legIdx" <*>
        m .: "distance"
        where
            t = parseTime <$> m .: "time"

instance ToNamedRecord TickRow where
    toNamedRecord TickRow{..} =
        unions [local, dummyTrack]
        where
            local =
                namedRecord
                    [ namedField "fixIdx" fixIdx
                    , namedField "tickLead" tickLead
                    , namedField "tickRace" tickRace
                    , namedField "zoneIdx" zoneIdx
                    , namedField "legIdx" legIdx
                    , namedField "distance" (f distance)
                    , namedField "area" (g area)
                    ]

            -- NOTE: Fields in TrackRow that are not in TickRow.
            dummyTrack =
                namedRecord
                    [ namedField "time" ("" :: String)
                    , namedField "lat" ("" :: String)
                    , namedField "lng" ("" :: String)
                    ]

            f = unquote . unpack . encode
            g (LeadingAreaStep x) = f $ fromRational x

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "fixIdx" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "zoneIdx" <*>
        m .: "legIdx" <*>
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
            <$> filter (\TickRow{legIdx = LegIdx ix} -> ix >= start) xs

leadingArea
    :: (Int -> Leg)
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> [TickRow]
    -> [TickRow]

leadingArea _ _ _ _ _ [] = []

leadingArea _ _ _ Nothing _ xs = xs
leadingArea _ _ Nothing _ _ xs = xs
leadingArea _ Nothing _ _ _ xs = xs

leadingArea
    toLeg
    (Just (TaskDistance td))
    close@(Just (LeadClose (EssTime tt)))
    down
    arrival
    rows =
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
                (toLcTrack toLeg close down arrival rows)

        extraRow = maybeToList $ do
            lr <- lastRow
            e <- extra
            return $ lr{area = e}

toLcPoint :: (Int -> Leg) -> TickRow -> Maybe LcPoint
toLcPoint _ TickRow{tickLead = Nothing} = Nothing
toLcPoint toLeg TickRow{legIdx = (LegIdx ix), tickLead = Just (LeadTick t), distance} =
    Just LcPoint
        { leg = toLeg ix
        , mark = TaskTime . MkQuantity . toRational $ t
        , togo = DistanceToEss . MkQuantity . toRational $ distance
        }

-- | The time of last landing among all pilots, in seconds from first lead.
newtype LeadAllDown = LeadAllDown EssTime

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
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> [TickRow]
    -> LcTrack
toLcTrack toLeg tc td ta xs =
    x{seq = reverse seq}
    where
        x@LcSeq{seq} = toLcTrackRev toLeg tc td ta $ reverse xs

toLcTrackRev
    :: (Int -> Leg)
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> [TickRow]
    -> LcTrack

-- NOTE: Everyone has bombed and no one has lead out from the start.
toLcTrackRev _ Nothing _ _ _ = LcSeq{seq = [], extra = Nothing}
toLcTrackRev _ _ Nothing _ _ = LcSeq{seq = [], extra = Nothing}
toLcTrackRev _ _ _ _ [] = LcSeq{seq = [], extra = Nothing}

toLcTrackRev
    _
    (Just (LeadClose _))
    (Just (LeadAllDown _))
    (Just (LeadArrival _))
    (TickRow{tickLead = Nothing} : _) = LcSeq{seq = [], extra = Nothing}

toLcTrackRev
    toLeg
    (Just (LeadClose close))
    (Just (LeadAllDown down))
    Nothing
    xs@(TickRow{distance} : _) =
        -- NOTE: Everyone has landed out.
        LcSeq{seq = xs', extra = Just y}
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs

        y = landOutRow
                (min down close)
                (DistanceToEss . MkQuantity . toRational $ distance)

toLcTrackRev
    toLeg
    (Just (LeadClose _))
    (Just (LeadAllDown _))
    (Just (LeadArrival arrive))
    xs@(TickRow{tickLead = Just (LeadTick t), distance} : _)
        -- NOTE: If distance <= 0 then ESS was made.
        | distance <= 0 = LcSeq{seq = xs', extra = Nothing}
        -- NOTE: Landed out before the last pilot made ESS.
        | t' < arrive = LcSeq{seq = xs', extra = Just yEarly}
        -- NOTE: Landed out after the last pilot made ESS.
        | otherwise = LcSeq{seq = xs', extra = Just yLate}
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs
        t' = EssTime $ toRational t

        yEarly =
                landOutRow
                    arrive
                    (DistanceToEss . MkQuantity . toRational $ distance)

        yLate =
                landOutRow
                    t'
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
timeToTick TimeRow{..} =
    TickRow
        { fixIdx = fixIdx
        , tickLead = tickLead
        , tickRace = tickRace
        , zoneIdx = zoneIdx
        , legIdx = legIdx
        , distance = distance
        , area = LeadingAreaStep 0
        }

discard
    :: (Int -> Leg)
    -> Maybe (QTaskDistance Double [u| m |])
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> Vector TimeRow
    -> Vector TickRow
discard toLeg dRace close down arrival xs =
    V.fromList
    . leadingArea toLeg dRace close down arrival
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

commentOnFixRange :: Pilot -> [FixIdx] -> String
commentOnFixRange pilot [] =
    "*NO* 'flying' fixes for " ++ show pilot
commentOnFixRange pilot xs =
    case [fix0 .. fixN] \\ xs of
        [] ->
            "["
            ++ show fix0
            ++ ".."
            ++ show fixN
            ++ "] fixes for "
            ++ show pilot

        ys ->
            "From ["
            ++ show fix0
            ++ ".."
            ++ show fixN
            ++ "] "
            ++ show pilot
            ++ " is missing fixes "
            ++ show (asRanges ys)
    where
        fix0 = minimum xs
        fixN = maximum xs