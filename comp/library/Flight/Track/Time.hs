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
    , leadingAreaSum
    , minLeadingCoef
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
import Data.UnitsOfMeasure ((+:), u, convert, zero, toRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList)

import Flight.Units ()
import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.LatLng.Raw (RawLat, RawLng, RawAlt)
import Flight.Score
    ( LeadingArea(..)
    , LeadingCoef(..)
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

-- WARNING: I suspect cassava doesn't support repeated column names.

-- | To enable easy CSV comparison, each CSV file has the same headers but for
-- some files these columns are empty of data. The file comparisons I'm
-- interested in are unpack-track with align-time and align-time with
-- discard-further.
allHeaders :: [String]
allHeaders =
    ["fixIdx", "time", "lat", "lng", "alt"]
    ++ ["tickLead", "tickRace", "zoneIdx", "legIdx", "togo"]
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
        , alt :: RawAlt
        -- ^ Altitude of the fix
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
        , alt :: RawAlt
        -- ^ Altitude of the fix
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , tickRace :: Maybe RaceTick
        -- ^ Seconds from first start.
        , zoneIdx :: ZoneIdx
        -- ^ The fix number for this leg.
        , legIdx :: LegIdx
        -- ^ Leg of the task.
        , togo :: Double
        -- ^ The distance yet to go to make goal in km.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

betweenTimeRow :: UTCTime -> UTCTime -> TimeRow -> Bool
betweenTimeRow t0 t1 TimeRow{time = t} =
    t0 <= t && t <= t1 

instance FlyClipping UTCTime [TimeRow] where
    clipToCut x@FlyCut{cut = Nothing} =
        x{uncut = []}

    clipToCut x@FlyCut{cut = Just (t0, t1), uncut = xs} =
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
        , togo :: Double
        -- ^ The distance yet to go to make goal in km.
        , area :: LeadingArea (Quantity Double [u| (km^2)*s |])
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
        unions
            [ local
            , toNamedRecord lat
            , toNamedRecord lng
            , toNamedRecord alt
            , dummyTime
            , dummyTick
            ]
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
                    , namedField "togo" ("" :: String)
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
        m .: "lng" <*>
        m .: "alt"
        where
            t = parseTime <$> m .: "time"

instance ToNamedRecord TimeRow where
    toNamedRecord TimeRow{..} =
        unions
            [ local
            , toNamedRecord lat
            , toNamedRecord lng
            , toNamedRecord alt
            , dummyTick
            ]
        where
            local =
                namedRecord
                    [ namedField "fixIdx" fixIdx
                    , namedField "tickLead" tickLead
                    , namedField "tickRace" tickRace
                    , namedField "time" time'
                    , namedField "zoneIdx" zoneIdx
                    , namedField "legIdx" legIdx
                    , namedField "togo" d
                    ]

            -- NOTE: Fields in TickRow that are not in TimeRow.
            dummyTick =
                namedRecord
                    [ namedField "area" ("" :: String)
                    ]

            time' = unquote . unpack . encode $ time
            d = unquote . unpack . encode $ togo

instance FromNamedRecord TimeRow where
    parseNamedRecord m =
        TimeRow <$>
        m .: "fixIdx" <*>
        t <*>
        m .: "lat" <*>
        m .: "lng" <*>
        m .: "alt" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "zoneIdx" <*>
        m .: "legIdx" <*>
        m .: "togo"
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
                    , namedField "togo" (f togo)
                    , namedField "area" area
                    ]

            -- NOTE: Fields in TrackRow that are not in TickRow.
            dummyTrack =
                namedRecord
                    [ namedField "time" ("" :: String)
                    , namedField "lat" ("" :: String)
                    , namedField "lng" ("" :: String)
                    , namedField "alt" ("" :: String)
                    ]

            f = unquote . unpack . encode

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "fixIdx" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "zoneIdx" <*>
        m .: "legIdx" <*>
        m .: "togo" <*>
        m .: "area"

minLeadingCoef
    :: [LeadingCoef (Quantity Double [u| 1 |])]
    -> Maybe (LeadingCoef (Quantity Double [u| 1 |]))
minLeadingCoef xs =
    if null xs then Nothing else Just $ minimum xs

-- TODO: The GAP guide says that the best distance for zeroth time is the
-- leading distance. Describe how this interacts with the distance to goal of
-- the first point on course.
leadingAreaSum
    :: Maybe LengthOfSs
    -> SpeedSection
    -> [TickRow]
    -> Maybe (LeadingArea (Quantity Double [u| (km^2)*s |]))
leadingAreaSum Nothing _ _ = Nothing
leadingAreaSum _ _ [] = Nothing
leadingAreaSum (Just _) Nothing xs =
    Just . LeadingArea $ sum' ys
    where
        ys =
            (\TickRow{area = LeadingArea a} -> a)
            <$> xs
leadingAreaSum (Just _) (Just (start, _)) xs =
    if null ys then Nothing else
    Just . LeadingArea $ sum' ys
    where
        ys =
            (\TickRow{area = LeadingArea a} -> a)
            <$> filter (\TickRow{legIdx = LegIdx ix} -> ix >= start) xs

sum' :: [Quantity Double u] -> Quantity Double u
sum' = foldr (+:) zero

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
toLcPoint toLeg TickRow{legIdx = (LegIdx ix), tickLead = Just (LeadTick t), togo} =
    Just LcPoint
        { leg = toLeg ix
        , mark = TaskTime . MkQuantity . toRational $ t
        , togo = DistanceToEss . MkQuantity . toRational $ togo
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
    xs@(TickRow{togo} : _) =
        -- NOTE: Everyone has landed out.
        LcSeq{seq = xs', extra = Just y}
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs

        y = landOutRow
                (min down close)
                (DistanceToEss . MkQuantity . toRational $ togo)

toLcTrackRev
    toLeg
    (Just (LeadClose _))
    (Just (LeadAllDown _))
    (Just (LeadArrival arrive))
    xs@(TickRow{tickLead = Just (LeadTick t), togo} : _)
        -- NOTE: If distance to go <= 0 then ESS was made.
        | togo <= 0 = LcSeq{seq = xs', extra = Nothing}
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
                    (DistanceToEss . MkQuantity . toRational $ togo)

        yLate =
                landOutRow
                    t'
                    (DistanceToEss . MkQuantity . toRational $ togo)

landOutRow :: EssTime -> DistanceToEss -> LcPoint
landOutRow (EssTime t) d =
    LcPoint
        { leg = LandoutLeg 0
        , mark = TaskTime $ MkQuantity t
        , togo = d
        }

taskToLeading :: QTaskDistance Double [u| m |] -> LengthOfSs
taskToLeading (TaskDistance d) =
    LengthOfSs . toRational' $ (convert d :: Quantity Double [u| km |])

timeToTick :: TimeRow -> TickRow
timeToTick TimeRow{..} =
    TickRow
        { fixIdx = fixIdx
        , tickLead = tickLead
        , tickRace = tickRace
        , zoneIdx = zoneIdx
        , legIdx = legIdx
        , togo = togo
        , area = LeadingArea zero
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

-- | Drop any rows where the distance togo is zero.
dropZeros :: [TickRow] -> [TickRow]
dropZeros =
    dropWhile ((== 0) . d)
    where
        d = togo :: (TickRow -> Double)

-- | Discard fixes further from goal than any previous fix.
discardFurther :: [TickRow] -> [TickRow]
discardFurther (x : y : ys)
    | d x <= d y = discardFurther (x : ys)
    | otherwise = x : discardFurther (y : ys)
    where
        d = togo :: (TickRow -> Double)
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
