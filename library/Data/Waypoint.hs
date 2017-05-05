{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Waypoint
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the IGC format for waypoint fixes. The date header is also parsed
as it is needed for the fixes that have only a time and pickup the date in the file
header.
-}
module Data.Waypoint
    (
    -- * Data
      IgcRecord(..)
    , HMS(..)
    , Lat(..)
    , Lng(..)
    , AltBaro(..)
    , AltGps(..)
    -- * Parsing Functions
    , parse
    , parseFromFile
    -- * Types
    , Altitude
    , Degree
    , Hour
    , Minute
    , Second
    ) where

import Data.List (partition)
import Text.Parsec.Char (endOfLine, anyChar)
import Text.ParserCombinators.Parsec
    ( GenParser
    , ParseError
    , (<|>)
    , char
    , string
    , many
    , manyTill
    , lookAhead
    , oneOf
    , noneOf
    , count
    , digit
    , eof
    , optionMaybe
    , runParser
    )
import qualified Text.ParserCombinators.Parsec as P

-- | An altitude in metres
type Altitude = String

-- | An hour of time.
type Hour = String

-- | A minute of time or a minute of a degree. If a minute of a degree, the
-- first two chars are whole minutes and the following chars are the decimal
-- part. No decimal point character is included.
type Minute = String

-- | A second of time.
type Second = String

-- | A whole degree of angle. May have leading zeros. Has no decimal part.
type Degree = String

-- | A time with hours, minutes and seconds.
data HMS = HMS Hour Minute Second

-- | A latitude with degrees and minutes.
data Lat
    = LatN Degree Minute -- ^ North
    | LatS Degree Minute -- ^ South

-- | A longitude with degrees and minutes.
data Lng
    = LngW Degree Minute -- ^ West
    | LngE Degree Minute -- ^ East

-- | Pressure altitude in metres
newtype AltBaro = AltBaro Altitude

-- | GPS altitude in metres
newtype AltGps = AltGps Altitude

-- |
-- The record types:
--
-- * A FR manufacturer and identification
-- * B Fix
-- * C Task/declaration
-- * D Differential GPS
-- * E Event
-- * F Constellation
-- * G Security
-- * H File header
-- * I List of extension data included at end of each fix B record
-- * J List of data included in each extension (K) Record
-- * K Extension data
-- * L Logbook/comments
-- * M, N, etc. - Spare
--
-- SOURCE: <http://carrier.csi.cam.ac.uk/forsterlewis/soaring/igc_file_format/igc_format_2008.html>
data IgcRecord
    -- | A location fix.
    = B HMS Lat Lng AltBaro (Maybe AltGps)
    -- | The date header record.
    | HFDTE String String String
    -- | Any other record type is ignored.
    | Ignore
    deriving Show

showDegree :: String -> String
showDegree d = d ++ "°"

showMinute :: String -> String
showMinute (m0 : m1 : m) = [m0, m1] ++ "." ++ m ++ "'"
showMinute m = m 

showHMS :: HMS -> String
showHMS (HMS hh mm ss) = hh ++ ":" ++ mm ++ ":" ++ ss

showLat :: Lat -> String
showLat (LatN d m) = showDegree d ++ " " ++ showMinute m ++ " N"
showLat (LatS d m) = showDegree d ++ " " ++ showMinute m ++ " S"

showLng :: Lng -> String
showLng (LngW d m) = showDegree d ++ " " ++ showMinute m ++ " W"
showLng (LngE d m) = showDegree d ++ " " ++ showMinute m ++ " E"

ltrimZero :: String -> String
ltrimZero = dropWhile ('0' ==)

instance Show HMS where
    show = showHMS

instance Show Lat where
    show = showLat

instance Show Lng where
    show = showLng

instance Show AltBaro where
    show (AltBaro x) = ltrimZero x ++ "m"

instance Show AltGps where
    show (AltGps x) = ltrimZero x ++ "m"

showIgc :: [ IgcRecord ] -> String
showIgc xs =
    unlines $ f <$> xs
    where
        f x = case x of
                   B{} -> "B"
                   _ -> show x

showIgcSummarize :: [ IgcRecord ] -> String
showIgcSummarize xs =
    (\(bs, ys) -> showIgc ys ++ summarize bs) $ partition isB xs
    where
        summarize [] = "no B records"
        summarize [ x ] = unlines [ show x, "... and no other B records" ]
        summarize (x : y : _) = unlines [ show x
                                        , show y
                                        ,"... plus " ++ show (length xs) ++ " other B records"
                                        ]

instance {-# OVERLAPPING #-} Show [ IgcRecord ] where
    show = showIgcSummarize


isB :: IgcRecord -> Bool
isB B{} = True
isB HFDTE{} = False
isB Ignore = False

igcFile :: GenParser Char st [IgcRecord]
igcFile = do
    hfdte <- manyTill anyChar (lookAhead (string "HFDTE")) *> headerLine
    lines' <- manyTill anyChar (char 'B') *> many line
    _ <- eof
    return $ hfdte : lines'

headerLine :: GenParser Char st IgcRecord
headerLine = do
    line' <- date
    _ <- endOfLine
    return line'

line :: GenParser Char st IgcRecord
line = do
    line' <- fix <|> ignore
    _ <- endOfLine
    return line'

hms :: GenParser Char st HMS
hms = do
    hh <- count 2 digit
    mm <- count 2 digit
    ss <- count 2 digit
    return $ HMS hh mm ss
       
lat :: GenParser Char st Lat
lat = do
    degs <- count 2 digit
    mins <- count 5 digit
    f <- const LatN <$> char 'N' <|> const LatS <$> char 'S'
    return $ f degs mins
       
lng :: GenParser Char st Lng
lng = do
    degs <- count 3 digit
    mins <- count 5 digit
    f <- const LngW <$> char 'W' <|> const LngE <$> char 'E'
    return $ f degs mins

altBaro :: GenParser Char st AltBaro
altBaro = AltBaro <$> count 5 digit
       
altGps :: GenParser Char st AltGps
altGps = AltGps <$> count 5 digit
       
alt :: GenParser Char st (AltBaro, Maybe AltGps)
alt = do
    _ <- oneOf "AV"
    altBaro' <- altBaro
    altGps' <- optionMaybe altGps
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
fix :: GenParser Char st IgcRecord
fix = do
    _ <- char 'B'
    hms' <- hms
    lat' <- lat
    lng' <- lng
    (altBaro', altGps') <- alt
    _ <- many (noneOf "\n")
    return $ B hms' lat' lng' altBaro' altGps'

date :: GenParser Char st IgcRecord
date = do
    _ <- string "HFDTE"
    dd <- count 2 digit
    mm <- count 2 digit
    yy <- count 2 digit
    return $ HFDTE dd mm yy

ignore :: GenParser Char st IgcRecord
ignore = do
    _ <- many (noneOf "\n")
    return Ignore

-- |
-- >>> parse "B0200223321354S14756057EA0024400241000\n"
-- Right B 02:00:22 33° 21.354' S 147° 56.057' E 244m (Just 241m)
parse
    :: String -- ^ A string to parse.
    -> Either ParseError [IgcRecord]
parse = P.parse igcFile "(stdin)"

-- |
-- >>> parseFromFile "./YMDCXXXF.IGC"
-- HFDTE "09" "01" "12"
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- Ignore
-- B 02:00:22 33° 21.354' S 147° 56.057' E 244m (Just 241m)
-- B 02:00:30 33° 21.354' S 147° 56.057' E 244m (Just 241m)
-- ... plus 994 other B records
parseFromFile
    :: FilePath -- ^ An IGC file to parse.
    -> IO (Either ParseError [IgcRecord])
parseFromFile fname = do
    input <- readFile fname
    return (runParser igcFile () fname input)
