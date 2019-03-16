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
    , Minute(..)
    , Second(..)
    , Year(..)
    , Month(..)
    , Day(..)
    , Nth(..)
    , addHoursIgc
    , isMark
    , isFix
    ) where

import Prelude hiding (readFile)
import Text.Printf (printf)
import Data.List (partition)
import Test.Tasty.QuickCheck (Arbitrary(..), frequency, oneof)

-- | An altitude in metres
newtype Altitude = Altitude String
    deriving (Eq, Ord)

-- | An hour of time.
newtype Hour = Hour Int
    deriving (Eq, Ord)

-- | A minute of time or a minute of a degree. If a minute of a degree, the
-- first two chars are whole minutes and the following chars are the decimal
-- part. No decimal point character is included.
newtype Minute = Minute String
    deriving (Eq, Ord)

-- | A second of time.
newtype Second = Second Int
    deriving (Eq, Ord)

-- | A whole degree of angle. May have leading zeros. Has no decimal part.
newtype Degree = Degree String
    deriving (Eq, Ord)

-- | A time with hours, minutes and seconds.
data HMS = HMS Hour Minute Second
    deriving (Eq, Ord)

-- | A latitude with degrees and minutes.
data Lat
    = LatN Degree Minute -- ^ North
    | LatS Degree Minute -- ^ South
    deriving (Eq, Ord)

-- | A longitude with degrees and minutes.
data Lng
    = LngW Degree Minute -- ^ West
    | LngE Degree Minute -- ^ East
    deriving (Eq, Ord)

-- | Pressure altitude in metres
newtype AltBaro = AltBaro Altitude
    deriving (Eq, Ord)

-- | GPS altitude in metres
newtype AltGps = AltGps Altitude
    deriving (Eq, Ord)

-- | A two digit character year
newtype Year = Year Int
    deriving (Eq, Ord)

-- | A two digit character month
newtype Month = Month Int
    deriving (Eq, Ord)

-- | A two digit character year
newtype Day = Day Int
    deriving (Eq, Ord)

-- | A two digit character item index
newtype Nth = Nth String
    deriving (Eq, Ord)

data YMD = YMD {year :: Year, month :: Month, day :: Day}
    deriving (Eq, Ord)

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
    -- | Any other record type is ignored
    | Ignore
    deriving (Eq, Ord)

instance Show IgcRecord where
    show B{hms, pos = (lat', lng', altB, altG)} =
         unwords
         [ show hms
         , show lat'
         , show lng'
         , show altB
         , "(" ++ show altG ++ ")"
         ]
    show (HFDTEDATE {ymd, nth = Nth n}) =
        concat [show ymd, ", ", n]
    show (HFDTE ymd) = show ymd
    show Ignore = ""

instance Arbitrary Hour where
    arbitrary = Hour <$> arbitrary

instance Arbitrary Minute where
    arbitrary = do
        h :: Int <- arbitrary
        return . Minute $ show h

instance Arbitrary Second where
    arbitrary = Second <$> arbitrary

instance Arbitrary HMS where
    arbitrary = do
        h <- arbitrary
        m <- arbitrary
        s <- arbitrary
        return $ HMS h m s

instance Arbitrary Lat where
    arbitrary =
        oneof
             [ do
                 d <- Degree <$> arbitrary
                 m <- Minute <$> arbitrary
                 return $ LatN d m

             , do
                 d <- Degree <$> arbitrary
                 m <- Minute <$> arbitrary
                 return $ LatS d m
            ]

instance Arbitrary Lng where
    arbitrary =
        oneof
             [ do
                 d <- Degree <$> arbitrary
                 m <- Minute <$> arbitrary
                 return $ LngE d m

             , do
                 d <- Degree <$> arbitrary
                 m <- Minute <$> arbitrary
                 return $ LngW d m
            ]

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
                     altGps <- (fmap $ AltGps . Altitude) <$> arbitrary
                     return $ B hms (lat, lng, altBaro, altGps)

            d1 = do
                     d <- Day <$> arbitrary
                     m <- Month <$> arbitrary
                     y <- Year <$> arbitrary
                     let ymd = YMD {year = y, month = m, day = d}
                     n <- Nth <$> arbitrary
                     return $ HFDTEDATE {ymd = ymd, nth = n}

            d2 = do
                     d <- Day <$> arbitrary
                     m <- Month <$> arbitrary
                     y <- Year <$> arbitrary
                     let ymd = YMD {year = y, month = m, day = d}
                     return $ HFDTE ymd

-- |
-- >>> addHoursHms (Hour 0) (HMS (Hour 0) (Minute "0") (Second 0))
-- 00:00:00
--
-- >>> addHoursHms (Hour 24) (HMS (Hour 12) (Minute "34") (Second 56))
-- 36:34:56
addHoursHms :: Hour -> HMS -> HMS
addHoursHms
    (Hour h)
    (HMS (Hour hh) mm ss) =
    HMS (Hour $ hh + h) mm ss

addHoursIgc :: Hour -> IgcRecord -> IgcRecord
addHoursIgc h x@B{hms} = x{hms = addHoursHms h hms}
addHoursIgc _ x = x

showDegree :: String -> String
showDegree d = d ++ "°"

showMinute :: String -> String
showMinute (m0 : m1 : m) = [m0, m1] ++ "." ++ m ++ "'"
showMinute m = m 

showHMS :: HMS -> String
showHMS (HMS (Hour hh) (Minute mm) (Second ss)) =
    printf "%02d:%02d:%02d" hh (read mm :: Int) ss

showLat :: Lat -> String
showLat (LatN (Degree d) (Minute m)) =
    showDegree d ++ " " ++ showMinute m ++ " N"
showLat (LatS (Degree d) (Minute  m)) =
    showDegree d ++ " " ++ showMinute m ++ " S"

showLng :: Lng -> String
showLng (LngW (Degree d) (Minute  m)) =
    showDegree d ++ " " ++ showMinute m ++ " W"
showLng (LngE (Degree d) (Minute  m)) =
    showDegree d ++ " " ++ showMinute m ++ " E"

ltrimZero :: String -> String
ltrimZero ('-' : s) =
    case '-' : ltrimZero s of
        "-0" -> "0"
        s' -> s'
ltrimZero s =
    case dropWhile ('0' ==) s of
        "" -> "0"
        s' -> s'

instance Show HMS where
    show = showHMS

instance Show Lat where
    show = showLat

instance Show Lng where
    show = showLng

instance Show AltBaro where
    show (AltBaro (Altitude x)) = ltrimZero x ++ "m"

instance Show AltGps where
    show (AltGps (Altitude x)) = ltrimZero x ++ "m"

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
isMark Ignore = False

-- | Is the record a __@B@__ record?
isFix :: IgcRecord -> Bool
isFix B{} = True
isFix HFDTEDATE{} = False
isFix HFDTE{} = False
isFix Ignore = False

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

