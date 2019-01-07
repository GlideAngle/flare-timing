{-|
Module: Flight.Igc
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Provides parsing the IGC format for waypoint fixes. The date header is also parsed
as it is needed for the fixes that have only a time and pickup the date in the file
header.
-}
module Flight.Igc
    (
    -- * Data
      IgcRecord(..)
    , HMS(..)
    , Lat(..)
    , Lng(..)
    , AltBaro(..)
    , AltGps(..)
    -- * Parsing
    , parse
    , parseFromFile
    -- * Types
    , Altitude(..)
    , Degree(..)
    , Hour(..)
    , Minute(..)
    , Second(..)
    , Year(..)
    , Month(..)
    , Day(..)
    , Nth(..)
    -- * Record classification
    , isMark
    , isFix
    ) where

import Prelude hiding (readFile)
import Data.ByteString.UTF8 (toString)
import Data.ByteString (readFile)
import Data.List (partition)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec as P (parse)
import Data.Void
import Data.Functor.Identity

-- | An altitude in metres
newtype Altitude = Altitude String
    deriving (Eq, Ord)

-- | An hour of time.
newtype Hour = Hour String
    deriving (Eq, Ord)

-- | A minute of time or a minute of a degree. If a minute of a degree, the
-- first two chars are whole minutes and the following chars are the decimal
-- part. No decimal point character is included.
newtype Minute = Minute String
    deriving (Eq, Ord)

-- | A second of time.
newtype Second = Second String
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
newtype Year = Year String
    deriving (Eq, Ord)

-- | A two digit character month
newtype Month = Month String
    deriving (Eq, Ord)

-- | A two digit character year
newtype Day = Day String
    deriving (Eq, Ord)

-- | A two digit character item index
newtype Nth = Nth String
    deriving (Eq, Ord)

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
    = B HMS Lat Lng AltBaro (Maybe AltGps)
    -- | The newer date header record
    | HFDTEDATE Day Month Year Nth
    -- | The older date header record
    | HFDTE Day Month Year
    -- | Any other record type is ignored
    | Ignore
    deriving (Eq, Ord)

instance Show IgcRecord where
    show (B t lat' lng' altB altG) =
         unwords
         [ show t
         , show lat'
         , show lng'
         , show altB
         , "(" ++ show altG ++ ")"
         ]
    show (HFDTEDATE (Day d) (Month m) (Year y) (Nth n)) =
        concat ["20", y, "-", m, "-", d, ", ", n]
    show (HFDTE (Day d) (Month m) (Year y)) =
        concat ["20", y, "-", m, "-", d]
    show Ignore = ""

showDegree :: String -> String
showDegree d = d ++ "°"

showMinute :: String -> String
showMinute (m0 : m1 : m) = [m0, m1] ++ "." ++ m ++ "'"
showMinute m = m 

showHMS :: HMS -> String
showHMS (HMS (Hour hh) (Minute mm) (Second ss)) =
    hh ++ ":" ++ mm ++ ":" ++ ss

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
ltrimZero = dropWhile ('0' ==)

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
        summarize (x : y : _) = unlines [ show x
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

igcFile :: ParsecT Void String Identity [IgcRecord]
igcFile = do
    hfdte <- try p1 <|> try p2
    lines' <- manyTill anySingle (char 'B') *> many line
    _ <- eof
    return $ hfdte : lines'
    where
        p1 = manyTill anySingle (lookAhead (string "HFDTEDATE:")) *> headerLine dateHFDTEDATE
        p2 = manyTill anySingle (lookAhead (string "HFDTE")) *> headerLine dateHFDTE
        headerLine date = do
            line' <- date
            _ <- eol
            return line'

line :: ParsecT Void String Identity IgcRecord
line = do
    line' <- fix <|> ignore
    _ <- eol
    return line'

-- |
-- >>> parseTest hms "0200223"
-- 02:00:22
hms :: ParsecT Void String Identity HMS
hms = do
    hh <- Hour <$> count 2 digitChar
    mm <- Minute <$> count 2 digitChar
    ss <- Second <$> count 2 digitChar
    return $ HMS hh mm ss

-- |
-- >>> parseTest lat "3321354S"
-- 33° 21.354' S
lat :: ParsecT Void String Identity Lat
lat = do
    degs <- Degree <$> count 2 digitChar
    mins <- Minute <$> count 5 digitChar
    f <- const LatN <$> char 'N' <|> const LatS <$> char 'S'
    return $ f degs mins

-- |
-- >>> parseTest lng "14756057E"
-- 147° 56.057' E
lng :: ParsecT Void String Identity Lng
lng = do
    degs <- Degree <$> count 3 digitChar
    mins <- Minute <$> count 5 digitChar
    f <- const LngW <$> char 'W' <|> const LngE <$> char 'E'
    return $ f degs mins

-- |
-- >>> parseTest altBaro "00244"
-- 244m
altBaro :: ParsecT Void String Identity AltBaro
altBaro = AltBaro . Altitude <$> count 5 digitChar

-- |
-- >>> parseTest altGps "00244"
-- 244m
altGps :: ParsecT Void String Identity AltGps
altGps = AltGps . Altitude <$> count 5 digitChar

-- |
-- >>> parseTest alt "A0024400241000"
-- (244m,Just 241m)
--
-- >>> parseTest alt "V0024400241000"
-- (244m,Just 241m)
alt :: ParsecT Void String Identity (AltBaro, Maybe AltGps)
alt = do
    _ <- oneOf ("AV" :: String)
    altBaro' <- altBaro
    altGps' <- optional altGps
    return (altBaro', altGps')

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

-- |
-- >>> parseTest fix "B0200223321354S14756057EA0024400241000\n"
-- 02:00:22 33° 21.354' S 147° 56.057' E 244m (Just 241m)
fix :: ParsecT Void String Identity IgcRecord
fix = do
    _ <- char 'B'
    hms' <- hms
    lat' <- lat
    lng' <- lng
    (altBaro', altGps') <- alt
    _ <- many (noneOf ("\n" :: String))
    return $ B hms' lat' lng' altBaro' altGps'

-- |
-- >>> parseTest dateHFDTEDATE "HFDTEDATE:030118,01"
-- 2018-01-03, 01
dateHFDTEDATE :: ParsecT Void String Identity IgcRecord
dateHFDTEDATE = do
    _ <- string "HFDTEDATE:"
    dd <- Day <$> count 2 digitChar
    mm <- Month <$> count 2 digitChar
    yy <- Year <$> count 2 digitChar
    _ <- string ","
    nn <- Nth <$> count 2 digitChar
    return $ HFDTEDATE dd mm yy nn

-- |
-- >>> parseTest dateHFDTE "HFDTE0301181"
-- 2018-01-03
dateHFDTE :: ParsecT Void String Identity IgcRecord
dateHFDTE = do
    _ <- string "HFDTE"
    dd <- Day <$> count 2 digitChar
    mm <- Month <$> count 2 digitChar
    yy <- Year <$> count 2 digitChar
    return $ HFDTE dd mm yy

ignore :: ParsecT Void String Identity IgcRecord
ignore = do
    _ <- many (noneOf ("\n" :: String))
    return Ignore

-- |
-- >>> line 1 igcHFDTE
-- "HFDTE030118\n"
-- >>> line 9 igcHFDTE
-- "B0405473321383S14756040EA0024800227\n"
-- >>> line 3527 igcHFDTE
-- "B0613553236449S14817636EA0036500338\n"
-- >>> line 3529 igcHFDTE
-- "LXGD Downloaded 2018-01-03  21:30:14\n"
--
-- >>> parse igcHFDTE
-- Right 2018-01-03
-- 04:05:47 33° 21.383' S 147° 56.040' E 248m (Just 227m)
-- 04:05:48 33° 21.388' S 147° 56.036' E 249m (Just 227m)
-- ... plus 3530 other B records
-- <BLANKLINE>
--
-- >>> line 1 igcHFDTEDATE
-- "HFDTEDATE:030118,01\n"
-- >>> line 9 igcHFDTEDATE
-- "B0312593321375S14755988EA0027700235\n"
-- >>> line 13179 igcHFDTEDATE
-- "B0657503228681S14842328EA0034500302\n"
-- >>> line 13181 igcHFDTEDATE
-- "LXGD Downloaded 2018-01-03  12:12:46\n"
--
-- >>> parse igcHFDTEDATE
-- Right 2018-01-03, 01
-- 03:13:00 33° 21.380' S 147° 55.984' E 283m (Just 237m)
-- 03:13:01 33° 21.385' S 147° 55.977' E 290m (Just 241m)
-- ... plus 13175 other B records
-- <BLANKLINE>
--
parse
   :: String -- ^ A string to parse
   -> Either (ParseErrorBundle String Void) [IgcRecord]
parse = P.parse igcFile "(stdin)"

parseFromFile
    :: FilePath -- ^ An IGC file to parse.
    -> IO (Either (ParseErrorBundle String Void) [IgcRecord])
parseFromFile fname =
    runParser igcFile fname . toString <$> readFile fname

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Language.Haskell.TH
-- >>> import Language.Haskell.TH.Syntax (lift)
-- >>> import Flight.Igc
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = lift =<< runIO readStr
-- :}
--
-- >>> line n = unlines . take 1 . drop n . lines
-- 
-- >>> fileHFDTEDATE  = "./test-suite-doctest/Sasha-Serebrennikova.20180103-121306.30169.72.igc"
-- >>> fileHFDTE  = "./test-suite-doctest/Brad-Porter.20180104-095852.36822.34.igc"
--
-- >>> igcHFDTEDATE = $(embedStr (System.IO.readFile fileHFDTEDATE))
-- >>> igcHFDTE = $(embedStr (System.IO.readFile fileHFDTE))

