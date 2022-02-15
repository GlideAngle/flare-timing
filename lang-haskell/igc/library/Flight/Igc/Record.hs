module Flight.Igc.Record
    ( IgcRecord(..)
    , YMD(..)
    , HMS(..)
    , Pos
    , Lat(..)
    , Lng(..)
    , AltBaro(..)
    , AltGps(..)
    , Altitude(..)
    , Degree(..)
    , Hour(..)
    , MinuteOfTime(..)
    , MinuteOfAngle(..)
    , Second(..)
    , Year(..)
    , Month(..)
    , Day(..)
    , Nth(..)
    , addHoursIgc
    , isMark
    , isFix
    ) where

import Prelude hiding (readFile, min)
import GHC.Generics (Generic)
import Control.DeepSeq
import Text.Printf (printf)
import Data.List (partition)
import Test.QuickCheck (Gen, Arbitrary(..), frequency, oneof)

-- | An altitude in metres.
newtype Altitude = Altitude Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | An hour of time.
newtype Hour = Hour Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A minute of time.
newtype MinuteOfTime = MinuteOfTime Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | Thousandths of a minute of angle.
newtype MinuteOfAngle = MinuteOfAngle {unThousandths :: Int}
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A second of time.
newtype Second = Second Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

instance Show Second where
    show (Second s) = showHmsForSecs s

showHmsForSecs :: Int -> String
showHmsForSecs sec =
    show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
    where
        show2i = printf "%02d"
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60

-- | A whole degree of angle. May have leading zeros. Has no decimal part.
newtype Degree = Degree Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A time with hours, minutes and seconds.
data HMS = HMS Hour MinuteOfTime Second
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A latitude with degrees and minutes.
data Lat
    = LatN Degree MinuteOfAngle -- ^ North
    | LatS Degree MinuteOfAngle -- ^ South
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A longitude with degrees and minutes.
data Lng
    = LngW Degree MinuteOfAngle -- ^ West
    | LngE Degree MinuteOfAngle -- ^ East
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | Pressure altitude in metres
newtype AltBaro = AltBaro Altitude
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | GPS altitude in metres
newtype AltGps = AltGps Altitude
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A two digit character year
newtype Year = Year Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A two digit character month
newtype Month = Month Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A two digit character year
newtype Day = Day Int
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

-- | A two digit character item index
newtype Nth = Nth String
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

data YMD = YMD {year :: Year, month :: Month, day :: Day}
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

instance Show YMD where
    show YMD{year = Year y, month = Month m, day = Day d} =
        concat
            [ "20"
            , printf "%02d" y
            , "-"
            , printf "%02d" m
            , "-"
            , printf "%02d" d
            ]

type Pos = (Lat, Lng, AltBaro, Maybe AltGps)

-- |
-- The record types are:
--
-- * A <http://www.gliding.ch/images/news/lx20/fichiers_igc.htm#Arec Manufacturer and identification>
-- * B <http://www.gliding.ch/images/news/lx20/fichiers_igc.htm#Brec Fix>
-- * C Task declaration
-- * D Differential GPS
-- * E Event
-- * F Constellation
-- * G Security
-- * H <http://www.gliding.ch/images/news/lx20/fichiers_igc.htm#Hrec File header>
-- * I List of extension data included at end of each fix B record
-- * J List of data included in each extension (K) Record
-- * K Extension data
-- * L Logbook comments
--
-- Other letters are spare for future record types.
data IgcRecord
    -- | A location fix
    = B {hms :: HMS, pos :: Pos}
    -- | The newer date header record
    | HFDTEDATE {ymd :: YMD, nth :: Nth}
    -- | The older date header record
    | HFDTE YMD
    -- | A digital signature
    | G
    -- | Any other record type is ignored
    | Ignore
    deriving (Eq, Ord, Generic)
    deriving anyclass NFData

instance Show IgcRecord where
    show B{hms, pos = (lat', lng', altB, altG)} =
         unwords
         [ show hms
         , show lat'
         , show lng'
         , show altB
         , "(" ++ show altG ++ ")"
         ]
    show HFDTEDATE{ymd, nth = Nth n} = concat [show ymd, ", ", n]
    show (HFDTE ymd) = show ymd
    show G = "G"
    show Ignore = "?"

instance Arbitrary Hour where
    arbitrary = Hour <$> arbitrary

instance Arbitrary Degree where
    arbitrary = Degree <$> arbitrary

instance Arbitrary MinuteOfTime where
    arbitrary = MinuteOfTime <$> arbitrary

instance Arbitrary MinuteOfAngle where
    arbitrary = MinuteOfAngle <$> arbitrary

instance Arbitrary Second where
    arbitrary = Second <$> arbitrary

instance Arbitrary HMS where
    arbitrary = HMS <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Lat where
    arbitrary =
        oneof
             [ LatN <$> arbitrary <*> arbitrary
             , LatS <$> arbitrary <*> arbitrary
             ]

instance Arbitrary Lng where
    arbitrary =
        oneof
             [ LngE <$> arbitrary <*> arbitrary
             , LngW <$> arbitrary <*> arbitrary
             ]

genYMD :: Gen YMD
genYMD = do
    d <- Day <$> arbitrary
    m <- Month <$> arbitrary
    y <- Year <$> arbitrary
    return $ YMD {year = y, month = m, day = d}

instance Arbitrary IgcRecord where
    arbitrary =
        frequency
            [ (9997, b)
            , (1, d1)
            , (1, d2)
            , (1, return Ignore)
            ]
        where
            b = do
                     hms <- arbitrary
                     lat <- arbitrary
                     lng <- arbitrary
                     altBaro <- AltBaro . Altitude <$> arbitrary
                     altGps <- fmap (AltGps . Altitude) <$> arbitrary
                     return $ B hms (lat, lng, altBaro, altGps)

            d1 = do
                     ymd <- genYMD
                     n <- Nth <$> arbitrary
                     return $ HFDTEDATE {ymd = ymd, nth = n}

            d2 = HFDTE <$> genYMD

-- |
-- >>> addHoursHms (Hour 0) (HMS (Hour 0) (MinuteOfTime 0) (Second 0))
-- 00:00:00
--
-- >>> addHoursHms (Hour 24) (HMS (Hour 12) (MinuteOfTime 34) (Second 56))
-- 36:34:56
addHoursHms :: Hour -> HMS -> HMS
addHoursHms
    (Hour h)
    (HMS (Hour hh) mm ss) =
    HMS (Hour $ hh + h) mm ss
{-# INLINABLE addHoursHms #-}

addHoursIgc :: Hour -> IgcRecord -> IgcRecord
addHoursIgc h x@B{hms} = x{hms = addHoursHms h hms}
addHoursIgc _ x = x
{-# INLINABLE addHoursIgc #-}

showDegreeOfLat :: Degree -> String
showDegreeOfLat (Degree d) = printf "%02d°" d

showDegreeOfLng :: Degree -> String
showDegreeOfLng (Degree d) = printf "%03d°" d

showMinute :: MinuteOfAngle -> String
showMinute (MinuteOfAngle thousandths) =
    printf "%06.3f'" $ (fromIntegral thousandths :: Double) / 1000

showHMS :: HMS -> String
showHMS (HMS (Hour hh) (MinuteOfTime mm) (Second ss)) =
    printf "%02d:%02d:%02d" hh mm ss

showLat :: Lat -> String
showLat (LatN d m) =
    showDegreeOfLat d ++ " " ++ showMinute m ++ " N"
showLat (LatS d m) =
    showDegreeOfLat d ++ " " ++ showMinute m ++ " S"

showLng :: Lng -> String
showLng (LngW d m) =
    showDegreeOfLng d ++ " " ++ showMinute m ++ " W"
showLng (LngE d m) =
    showDegreeOfLng d ++ " " ++ showMinute m ++ " E"

instance Show HMS where
    show = showHMS

instance Show Lat where
    show = showLat

instance Show Lng where
    show = showLng

instance Show AltBaro where
    show (AltBaro (Altitude x)) = show x ++ "m"

instance Show AltGps where
    show (AltGps (Altitude x)) = show x ++ "m"

showIgc :: [ IgcRecord ] -> String
showIgc xs =
    unlines $ f <$> filter (Ignore /=) xs
    where
        f x = case x of
                   B{} -> "B"
                   Ignore -> ""
                   _ -> show x

showIgcSummarize :: [ IgcRecord ] -> String
showIgcSummarize xs =
    (\(bs, ys) -> showIgc ys ++ summarize bs) $ partition isFix xs
    where
        summarize [] = "no B records"
        summarize [ x ] = unlines [ show x, "... and no other B records" ]
        summarize (x : y : _) =
            unlines
                [ show x
                , show y
                ,"... plus " ++ show (length xs) ++ " other B records"
                ]

instance {-# OVERLAPPING #-} Show [ IgcRecord ] where
    show = showIgcSummarize

-- | Is the record a mark in time? __@HFDTEDATE@__ and __@HFDTE@__ are dates in
-- UTC.  They mark time zero from which the hour minute and second of each
-- __@B@__ record is offset.
isMark :: IgcRecord -> Bool
isMark B{} = False
isMark HFDTEDATE{} = True
isMark HFDTE{} = True
isMark G{} = False
isMark Ignore = False
{-# INLINABLE isMark #-}

-- | Is the record a __@B@__ record?
isFix :: IgcRecord -> Bool
isFix B{} = True
isFix HFDTEDATE{} = False
isFix HFDTE{} = False
isFix G{} = False
isFix Ignore = False
{-# INLINABLE isFix #-}

{--
B: record type is a basic tracklog record
110135: <time> tracklog entry was recorded at 11:01:35 i.e. just after 11am
5206343N: <lat> i.e. 52 degrees 06.343 minutes North
00006198W: <long> i.e. 000 degrees 06.198 minutes West
A: <alt valid flag> confirming this record has a valid altitude value
00587: <altitude from pressure sensor>
00558: <altitude from GPS>
SEE: http://carrier.csi.cam.ac.uk/forsterlewis/soaring/igc_file_format/
--}

