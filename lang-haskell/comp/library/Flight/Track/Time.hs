{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
    , AreaRow(..)
    , LeadAllDown(..)
    , LeadArrival(..)
    , LeadClose(..)
    , FixIdx(..)
    , ZoneIdx(..)
    , LegIdx(..)
    , TimeToTick
    , TickToTick
    , LeadingAreas(..)
    , AreaSteps
    , leadingAreas
    , leadingAreaFlown
    , leadingAreaAfterLanding
    , leadingAreaBeforeStart
    , minLeadingCoef
    , taskToLeading
    , timesToKeptTicks
    , ticksToAreas

    , TimeHeader(..), timeHeader
    , TickHeader(..), tickHeader
    , AreaHeader(..), areaHeader

    , commentOnFixRange
    , copyTimeToTick
    , altBonusTimeToTick
    , altBonusTickToTick
    , glideRatio
    ) where

import Prelude hiding (seq)
import Data.Function ((&))
import Data.Maybe (fromMaybe, catMaybes)
import Data.Csv
    ( ToNamedRecord(..), FromNamedRecord(..)
    , ToField(..), FromField(..)
    , Header, (.:)
    , namedRecord, namedField
    )
import Data.List ((\\), findIndices)
import Data.List.Split (wordsBy)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.HashMap.Strict (unions)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.UnitsOfMeasure
    ( (+:), (-:), (*:), (/:), KnownUnit, Unpack
    , u, unQuantity, convert, zero, fromRational', toRational'
    )
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, toList, empty, null)

import Flight.Units ()
import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.LatLng (QAlt, Alt(..))
import Flight.LatLng.Raw (RawLat, RawLng, RawAlt(..))
import "flight-gap-allot" Flight.Score (Pilot)
import "flight-gap-stop" Flight.Score (GlideRatio(..))
import "flight-gap-lead" Flight.Score
    ( LeadingAreas(..)
    , LeadingArea(..)
    , LeadingAreaUnits
    , LeadingCoef(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcArea
    , LcPoint(..)
    , LcSeq(..)
    , LcTrack
    , LengthOfSs(..)
    , TaskDeadline(..)
    , EssTime(..)
    , LeadAllDown(..)
    , showSecs
    )
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Zone.MkZones (Discipline(..))
import Flight.Zone.SpeedSection (SpeedSection)
import Flight.Track.Range (asRanges)
import Data.Ratio.Rounding (dpRound)

type TimeToTick = TimeRow -> TickRow
type TickToTick = TickRow -> TickRow
type AreaSteps u = TaskDeadline -> LeadAllDown -> LengthOfSs -> LcSeq LcPoint -> LcArea u

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
    deriving anyclass (ToJSON, FromJSON, NFData)
    deriving newtype (Enum, Num, ToField, FromField)

-- | The index of a fix for a leg, that section of the tracklog between one
-- zone and the next.
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

newtype TimeHeader = TimeHeader Header
newtype TickHeader = TickHeader Header
newtype AreaHeader = AreaHeader Header

-- WARNING: I suspect cassava doesn't support repeated column names.

-- | To enable easy CSV comparison, some CSV files have the same headers but
-- for some files these columns are empty of data. The file comparisons I'm
-- interested in are unpack-track with align-time and align-time with
-- discard-further.
timeHeader :: TimeHeader
timeHeader = TimeHeader $ V.fromList ["fixIdx", "legIdx", "zoneIdx", "tickLead", "tickRace", "time", "lat", "lng", "alt", "togo"]

tickHeader :: TickHeader
tickHeader = TickHeader $ V.fromList ["fixIdx", "legIdx", "zoneIdx", "tickLead", "tickRace", "time", "lat", "lng", "alt", "togo"]

areaHeader :: AreaHeader
areaHeader = AreaHeader $ V.fromList ["fixIdx", "legIdx", "tickLead", "togo", "area"]

data TrackRow =
    TrackRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , time :: UTCTime
        -- ^ Time of the fix.
        , lat :: RawLat
        -- ^ Latitude of the fix.
        , lng :: RawLng
        -- ^ Longitude of the fix.
        , alt :: RawAlt
        -- ^ Altitude of the fix.
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

-- | A fix but with time as elapsed time from the first crossing time.
data TimeRow =
    TimeRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , time :: UTCTime
        -- ^ Time of the fix.
        , lat :: RawLat
        -- ^ Latitude of the fix.
        , lng :: RawLng
        -- ^ Longitude of the fix.
        , alt :: RawAlt
        -- ^ Altitude of the fix.
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
        , alt :: RawAlt
        -- ^ Altitude of the fix.
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
        }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A fix but with time as elapsed time from the first crossing time.
data AreaRow u =
    AreaRow
        { fixIdx :: FixIdx
        -- ^ The fix number for the whole track.
        , tickLead :: Maybe LeadTick
        -- ^ Seconds from first lead.
        , legIdx :: LegIdx
        -- ^ Leg of the task
        , togo :: Double
        -- ^ The distance yet to go to make goal in km.
        , area :: LeadingArea (LeadingAreaUnits u)
        -- ^ Leading coefficient area step.
        }
    deriving (Eq, Ord, Generic)

deriving instance (KnownUnit (Unpack u)) => Show (AreaRow u)
deriving instance (KnownUnit (Unpack u)) => ToJSON (AreaRow u)
deriving instance (KnownUnit (Unpack u)) => FromJSON (AreaRow u)

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
                    , namedField "alt" alt
                    , namedField "tickLead" tickLead
                    , namedField "tickRace" tickRace
                    , namedField "zoneIdx" zoneIdx
                    , namedField "legIdx" legIdx
                    , namedField "togo" (f togo)
                    ]

            -- NOTE: Fields in TrackRow that are not in TickRow.
            dummyTrack =
                namedRecord
                    [ namedField "time" ("" :: String)
                    , namedField "lat" ("" :: String)
                    , namedField "lng" ("" :: String)
                    , namedField "area" ("" :: String)
                    ]

            f = unquote . unpack . encode

instance FromNamedRecord TickRow where
    parseNamedRecord m =
        TickRow <$>
        m .: "fixIdx" <*>
        m .: "alt" <*>
        m .: "tickLead" <*>
        m .: "tickRace" <*>
        m .: "zoneIdx" <*>
        m .: "legIdx" <*>
        m .: "togo"

instance ToNamedRecord (AreaRow u) where
    toNamedRecord AreaRow{..} =
        unions [local, dummyTrack]
        where
            local =
                namedRecord
                    [ namedField "fixIdx" fixIdx
                    , namedField "tickLead" tickLead
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
                    ]

            f = unquote . unpack . encode

instance FromNamedRecord (AreaRow u) where
    parseNamedRecord m =
        AreaRow <$>
        m .: "fixIdx" <*>
        m .: "tickLead" <*>
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
leadingAreaFlown
    :: Maybe LengthOfSs
    -> SpeedSection
    -> [AreaRow u]
    -> Maybe (LeadingArea (LeadingAreaUnits u))
leadingAreaFlown Nothing _ _ = Nothing
leadingAreaFlown _ _ [] = Nothing
leadingAreaFlown (Just _) Nothing xs =
    Just . LeadingArea $ sum' ys
    where
        ys =
            (\AreaRow{area = LeadingArea a} -> a)
            <$> xs
leadingAreaFlown (Just _) (Just (start, _)) xs =
    if null ys then Nothing else
    Just . LeadingArea $ sum' ys
    where
        ys =
            (\AreaRow{area = LeadingArea a} -> a)
            <$> filter (\AreaRow{legIdx = LegIdx ix} -> ix >= start) xs

leadingAreaAfterLanding :: _ -> LeadAllDown -> LcPoint -> Maybe (LeadingArea (LeadingAreaUnits u))
leadingAreaAfterLanding f (LeadAllDown (EssTime t)) LcPoint{togo = DistanceToEss d}
    | t' <= zero = Nothing
    | d <= zero = Nothing
    | otherwise = Just . LeadingArea . fromRational' $ f d t'
    where
        t' = MkQuantity t

leadingAreaBeforeStart :: _ -> LcPoint -> Maybe (LeadingArea (LeadingAreaUnits u))
leadingAreaBeforeStart f LcPoint{mark = TaskTime t, togo = DistanceToEss d}
    | t <= zero = Nothing
    | d <= zero = Nothing
    | otherwise = Just . LeadingArea . fromRational' $ f d t

sum' :: [Quantity Double u] -> Quantity Double u
sum' = foldr (+:) zero

data LeadingLanding
    = LandedAfterEss -- ^ ESS was made.
    | LandedBeforeLastEss (Maybe LcPoint) -- ^ Landed out before the last pilot made ESS.
    | LandedAfterLastEss (Maybe LcPoint) -- ^ Landed out after the last pilot made ESS.
    | LandedOutEveryPilot (Maybe LcPoint) -- ^ Landed out but so did everyone else.
    | LandedNoLeaders -- ^ Can't identify any leadings pilots.

tickToArea :: TickRow -> AreaRow u
tickToArea TickRow{..} =
    AreaRow
        { fixIdx = fixIdx
        , tickLead = tickLead
        , legIdx = legIdx
        , togo = togo
        , area = LeadingArea zero
        }

leadingAreas
    :: AreaSteps (LeadingAreaUnits u)
    -> (Int -> Leg)
    -> Maybe LengthOfSs
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> Vector TickRow
    -> LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint)

leadingAreas _ _ _ _ Nothing _ xs = LeadingAreas (tickToArea <$> xs) Nothing Nothing
leadingAreas _ _ _ Nothing _ _ xs = LeadingAreas (tickToArea <$> xs) Nothing Nothing
leadingAreas _ _ Nothing _ _ _ xs = LeadingAreas (tickToArea <$> xs) Nothing Nothing

leadingAreas
    areaSteps
    toLeg
    (Just lengthOfSs)
    close@(Just (LeadClose (EssTime tt)))
    down@(Just leadAllDown)
    arrival
    xs
        | V.null xs = LeadingAreas V.empty Nothing Nothing
        | otherwise =
            LeadingAreas
                { areaFlown = V.fromList flown
                , areaAfterLanding = afterLanding
                , areaBeforeStart = beforeStart
                }
            where
                ys = V.toList xs
                (landing, lcTrack@LcSeq{seq = lcPoints}) = toLcTrack toLeg close down arrival ys

                LeadingAreas{areaFlown = LcSeq{seq = areas}} :: LcArea _ =
                    areaSteps
                        (TaskDeadline $ MkQuantity tt)
                        leadAllDown
                        lengthOfSs
                        lcTrack

                flown =
                    filter keepArea
                    [
                        AreaRow
                            { fixIdx = fixIdx
                            , tickLead = tickLead
                            , legIdx = legIdx
                            , togo = togo
                            , area = a
                            }
                    | TickRow{..} <- ys
                    | a <- areas
                    ]

                afterLanding =
                    case landing of
                        LandedAfterEss -> Nothing
                        LandedBeforeLastEss x -> x
                        LandedAfterLastEss x -> x
                        LandedOutEveryPilot x -> x
                        LandedNoLeaders -> Nothing

                keepLeg leg = case leg of RaceLeg _ -> True; _ -> False
                keepPoint LcPoint{leg} = keepLeg leg
                keepArea AreaRow{legIdx = LegIdx i} = keepLeg $ toLeg i

                beforeStart =
                    case filter keepPoint lcPoints of
                        (y : _) -> Just y
                        _ -> Nothing

toLcPoint :: (Int -> Leg) -> TickRow -> Maybe LcPoint
toLcPoint _ TickRow{tickLead = Nothing} = Nothing
toLcPoint toLeg TickRow{legIdx = (LegIdx ix), tickLead = Just (LeadTick t), togo} =
    Just LcPoint
        { leg = toLeg ix
        , mark = TaskTime . MkQuantity . toRational $ t
        , togo = DistanceToEss . MkQuantity . toRational $ togo
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
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> [TickRow]
    -> (LeadingLanding, LcTrack)
toLcTrack toLeg tc td ta xs =
    (ll, x{seq = reverse seq})
    where
        (ll, x@LcSeq{seq}) = toLcTrackRev toLeg tc td ta $ reverse xs

toLcTrackRev
    :: (Int -> Leg)
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> [TickRow]
    -> (LeadingLanding, LcTrack)

-- NOTE: Everyone has bombed and no one has lead out from the start.
toLcTrackRev _ Nothing _ _ _ = (LandedNoLeaders, LcSeq{seq = []})
toLcTrackRev _ _ Nothing _ _ = (LandedNoLeaders, LcSeq{seq = []})
toLcTrackRev _ _ _ _ [] = (LandedNoLeaders, LcSeq{seq = []})

toLcTrackRev
    _
    (Just (LeadClose _))
    (Just (LeadAllDown _))
    (Just (LeadArrival _))
    (TickRow{tickLead = Nothing} : _) = (LandedNoLeaders, LcSeq{seq = []})

toLcTrackRev
    toLeg
    (Just _)
    (Just _)
    Nothing
    xs@(x : _) =
        (LandedOutEveryPilot y, LcSeq{seq = xs'})
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs
        y = toLcPoint toLeg x

toLcTrackRev
    toLeg
    (Just (LeadClose _))
    (Just (LeadAllDown _))
    (Just (LeadArrival arrive))
    xs@(x@TickRow{tickLead = Just (LeadTick t), togo} : _)
        | togo <= 0 = (LandedAfterEss, LcSeq{seq = xs'})
        | t' < arrive = (LandedBeforeLastEss y, LcSeq{seq = xs'})
        | otherwise = (LandedAfterLastEss y, LcSeq{seq = xs'})
    where
        xs' = catMaybes $ toLcPoint toLeg <$> xs
        t' = EssTime $ toRational t
        y = toLcPoint toLeg x

taskToLeading :: QTaskDistance Double [u| m |] -> LengthOfSs
taskToLeading (TaskDistance d) =
    LengthOfSs . toRational' $ (convert d :: Quantity Double [u| km |])

copyTimeToTick :: TimeToTick
copyTimeToTick TimeRow{..} =
    TickRow
        { fixIdx = fixIdx
        , alt = alt
        , tickLead = tickLead
        , tickRace = tickRace
        , zoneIdx = zoneIdx
        , legIdx = legIdx
        , togo = togo
        }

glideRatio :: Discipline -> GlideRatio
glideRatio hgOrPg =
    GlideRatio $ hgOrPg & \case HangGliding -> 5; Paragliding -> 4

altBonusTickToTick :: GlideRatio -> QAlt Double [u| m |] -> TickToTick
altBonusTickToTick (GlideRatio gr) (Alt qAltGoal) row@TickRow{alt = RawAlt a, ..} =
    if qAlt <= qAltGoal then row else
    TickRow
        { fixIdx = fixIdx
        , alt = RawAlt $ toRational alt'
        , tickLead = tickLead
        , tickRace = tickRace
        , zoneIdx = zoneIdx
        , legIdx = legIdx
        , togo = togo'
        }
    where
        qAlt :: Quantity Double [u| m |]
        qAlt = MkQuantity $ fromRational a

        diffAlt :: Quantity Double [u| m |]
        diffAlt= qAlt -: qAltGoal

        qTogo :: Quantity Double [u| km |]
        qTogo = MkQuantity togo

        diffTogo :: Quantity Double [u| m |]
        diffTogo = convert qTogo

        gr' = fromRational gr
        diffAltAsReach = gr' *: diffAlt
        diffTogoAsAlt = diffTogo /: gr'

        (alt', togo') =
            if diffTogo < diffAltAsReach then (unQuantity $ qAlt -: diffTogoAsAlt, 0) else
            let d :: Quantity Double [u| km |]
                d = convert $ diffTogo -: diffAltAsReach
            -- TODO: Stop rounding of togo in altBonus.
            in (unQuantity qAltGoal, fromRational . dpRound 3 . toRational $ unQuantity d)

altBonusTimeToTick :: GlideRatio -> QAlt Double [u| m |] -> TimeToTick
altBonusTimeToTick (GlideRatio gr) (Alt qAltGoal) row@TimeRow{alt = RawAlt a, ..} =
    if qAlt <= qAltGoal then copyTimeToTick row else
    TickRow
        { fixIdx = fixIdx
        , alt = RawAlt $ toRational alt'
        , tickLead = tickLead
        , tickRace = tickRace
        , zoneIdx = zoneIdx
        , legIdx = legIdx
        , togo = togo'
        }
    where
        qAlt :: Quantity Double [u| m |]
        qAlt = MkQuantity $ fromRational a

        diffAlt :: Quantity Double [u| m |]
        diffAlt= qAlt -: qAltGoal

        qTogo :: Quantity Double [u| km |]
        qTogo = MkQuantity togo

        diffTogo :: Quantity Double [u| m |]
        diffTogo = convert qTogo

        gr' = fromRational gr
        diffAltAsReach = gr' *: diffAlt
        diffTogoAsAlt = diffTogo /: gr'

        (alt', togo') =
            if diffTogo < diffAltAsReach then (unQuantity $ qAlt -: diffTogoAsAlt, 0) else
            let d :: Quantity Double [u| km |]
                d = convert $ diffTogo -: diffAltAsReach
            -- TODO: Stop rounding of togo in altBonus.
            in (unQuantity qAltGoal, fromRational . dpRound 3 . toRational $ unQuantity d)

ticksToAreas
    :: AreaSteps (LeadingAreaUnits u)
    -> (Int -> Leg)
    -> Maybe LengthOfSs
    -> Maybe LeadClose
    -> Maybe LeadAllDown
    -> Maybe LeadArrival
    -> Vector TickRow
    -> LeadingAreas (Vector (AreaRow u)) (Maybe LcPoint)
ticksToAreas areaSteps toLeg dRace close down arrival =
    (\LeadingAreas{areaFlown = af, areaBeforeStart = bs, areaAfterLanding = al} ->
        LeadingAreas
            { areaFlown = af
            , areaAfterLanding = al
            , areaBeforeStart = bs
            })
    . leadingAreas areaSteps toLeg dRace close down arrival

timesToKeptTicks
    :: TimeToTick
    -- ^ A function that converts the type of row. This is applied before rows
    -- are discarded.
    -> TickToTick
    -- ^ A function that converts rows after discarding.
    -> Vector TimeRow
    -> Vector TickRow
timesToKeptTicks timeToTick tickToTick =
    V.fromList
    . discardFurther
    . dropZeros
    . V.toList
    . fmap (tickToTick . timeToTick)

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
