{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module: Flight.Kml
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Provides parsing of dumped tracklogs. In hang gliding and paragliding
competitions when <http://fs.fai.org/ FS> and <http://www.gpsdump.no GpsDump>
are paired in competition mode a pilot's tracklog is dumped as
<https://developers.google.com/kml/ KML>. This is exlained in detail on the
<http://fs.fai.org/trac/wiki/GpsDump/ FS wiki>.
-}
module Flight.Kml
    (
    -- * Usage
    -- $use
    
    -- * The newtypes
      Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)

    -- * Typeclasses
    , T.LatLngAlt(..)
    , T.FixMark(..)

    -- * Tracklog as a list of marked fixes
    , LLA(..)
    , mkPosition
    , Fix(..)
    , MarkedFixes(..)

    -- ** Parsing
    , parse
    , parseTimeOffsets
    , parseBaroMarks
    , parseLngLatAlt
    , roundTripLatLngAlt
    , formatFloat

    -- ** Length and range
    , fixesLength
    , fixesSecondsRange
    , fixesUTCTimeRange

    -- ** Display of fixes
    , showFixesLength
    , showFixesSecondsRange
    , showFixesUTCTimeRange

    -- ** Display of a fix
    , showLatLngAlt
    , showLngLatAlt
    , showTimeAlt

    -- * GPSDump KML
    -- $kml
    ) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (&&&)
    , (>>>)
    , (/>)
    , (>>.)
    , runX
    , getText
    , getAttrValue
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , hasAttrValue
    , filterA
    , listA
    , arr
    , orElse
    , constA
    )
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.List (concatMap)
import Data.List.Split (splitOn)
import Text.Parsec (string, parserZero)
import Text.Parsec.Token as P
import Text.Parsec.Char (spaces, digit, char)
import Text.ParserCombinators.Parsec
    ( GenParser
    , (<?>)
    , eof
    , option
    , sepBy
    , count
    , noneOf
    , many
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)
import Numeric (showFFloat)
import qualified Flight.Types as T (LatLngAlt(..), FixMark(..))
import Flight.Types
    ( LLA(..)
    , Fix(..)
    , MarkedFixes(..)
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    , mkPosition
    , showTimeAlt
    , fixesLength
    , fixesSecondsRange
    , fixesUTCTimeRange
    , showFixesLength
    , showFixesSecondsRange
    , showFixesUTCTimeRange
    )

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Rational
pFloat = parsecMap toRational $ P.float lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

zipFixes :: [Seconds] -> [LLA] -> [Maybe Altitude] -> [Fix]
zipFixes = zipWith3 Fix

-- | Get the fixes. Some KML files don't have PressureAltitude.
getFix :: ArrowXml a => a XmlTree (Maybe MarkedFixes)
getFix =
    getTrack
    >>> (getFsInfo >>> getFirstTime)
    &&& listA getCoord
    &&& (getFsInfo >>> listA getTime)
    &&& (getFsInfo >>> (listA getBaro `orElse` constA []))
    >>> arr (\(t0, (cs, (ts, bs))) -> do
        t0' <- t0
        let bs' = if null bs then repeat Nothing else Just <$> bs
        let fs = zipFixes ts cs bs'
        return $ MarkedFixes t0' fs)
    where
        isMetadata =
            getChildren
            >>> hasName "Metadata"
            >>> hasAttrValue "type" (== "track")

        getTrack =
            getChildren
            >>> hasName "Document"
            /> hasName "Folder"
            /> hasName "Placemark"
            >>> filterA isMetadata
            >>. take 1

        getFirstTime =
            getAttrValue "time_of_first_point"
            >>> arr parseUtcTime

        getFsInfo =
            getChildren
            >>> hasName "Metadata"
            /> hasName "FsInfo"
            >>. take 1

        getTime =
            getChildren
            >>> hasName "SecondsFromTimeOfFirstPoint"
            /> getText
            >>. concatMap parseTimeOffsets

        getBaro =
            getChildren
            >>> hasName "PressureAltitude"
            /> getText
            >>. concatMap parseBaroMarks

        getCoord =
            getChildren
            >>> hasName "LineString"
            /> hasName "coordinates"
            /> getText
            >>. concatMap parseLngLatAlt

-- | Parse the tracklog from the KML string as 'MarkedFixes'.
-- 
-- >>> Right MarkedFixes{mark0, fixes} <- parse kml
-- >>> mark0
-- 2012-01-14 02:12:55 UTC
-- >>> length fixes
-- 6547
-- >>> head fixes
-- Fix {fixMark = sec=0, fix = LLA {llaLat = lat=-33.36160000, llaLng = lng=147.93205000, llaAltGps = alt=237}, fixAltBaro = Just alt=239}
-- >>> last fixes
-- Fix {fixMark = sec=13103, fix = LLA {llaLat = lat=-33.65073300, llaLng = lng=147.56036700, llaAltGps = alt=214}, fixAltBaro = Just alt=238}
parse :: String -> IO (Either String MarkedFixes)
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getFix
    return
        $ case xs of
            [Nothing] -> Left "Couldn't parse the marked fixes"
            [Just x] -> Right x
            _ -> Left $
                    "Expected 1 set of marked fixes but got "
                    ++ show (length xs)
                    ++ "."

pNats :: GenParser Char st [Integer]
pNats = do
    _ <- spaces
    xs <- pNat `sepBy` spaces
    _ <- eof
    return xs

parseUtcTime :: String -> Maybe UTCTime
parseUtcTime s =
    case P.parse pUtcTimeZ "(stdin)" s of
        Left _ -> Nothing
        Right t -> Just t

pUtcTimeZ :: GenParser Char st UTCTime
pUtcTimeZ = do
    ymd <- many $ noneOf "T"
    _ <- char 'T'
    hrs <- count 2 digit
    _ <- char ':'
    mins <- count 2 digit
    _ <- char ':'
    secs <- count 2 digit
    zulu <- option "Z" (string "Z")

    let s = mconcat [ymd, "T", hrs, ":", mins, ":", secs, zulu]
    let t = parseTimeM False defaultTimeLocale "%FT%TZ" s

    case t of
        Nothing -> parserZero
        Just t' -> return t'

-- | Parse the list of time offsets.
-- 
-- >>> parseTimeOffsets "0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30"
-- [sec=0,sec=2,sec=4,sec=6,sec=8,sec=10,sec=12,sec=14,sec=16,sec=18,sec=20,sec=22,sec=24,sec=26,sec=28,sec=30]
parseTimeOffsets :: String -> [Seconds]
parseTimeOffsets s =
    case P.parse pNats "(stdin)" s of
        Left _ -> []
        Right xs -> Seconds <$> xs

-- | Parse the list of barometric pressure altitudes.
-- 
-- >>> parseBaroMarks "239 240 240 239 239 239 239 239 239 240 239 240 239 239 240"
-- [alt=239,alt=240,alt=240,alt=239,alt=239,alt=239,alt=239,alt=239,alt=239,alt=240,alt=239,alt=240,alt=239,alt=239,alt=240]
parseBaroMarks :: String -> [Altitude]
parseBaroMarks s =
    case P.parse pNats "(stdin)" s of
         Left _ -> []
         Right xs -> Altitude <$> xs

pFix :: GenParser Char st (Rational, Rational, Integer)
pFix = do
    -- NOTE: KML coordinates have a space between tuples.
    -- lon,lat[,alt]
    -- SEE: https://developers.google.com/kml/documentation/kmlreference#linestring
    lngSign <- option id $ const negate <$> char '-'
    lng <- pFloat <?> "No longitude"
    _ <- char ','
    latSign <- option id $ const negate <$> char '-'
    lat <- pFloat <?> "No latitude"
    _ <- char ','
    altSign <- option id $ const negate <$> char '-'
    alt <- pNat <?> "No altitude"
    return (latSign lat, lngSign lng, altSign alt)

pFixes :: GenParser Char st [ (Rational, Rational, Integer) ]
pFixes = do
    _ <- spaces
    xs <- pFix `sepBy` spaces <?> "No fixes"
    _ <- eof
    return xs

-- | Avoids __@"0."@__ because ...
-- 
-- @
-- > (read "0." :: Double)
-- Exception: Prelude.read: no parse
-- > (read "0.0" :: Double)
-- 0.0
-- @
--
-- >>> formatFloat "112.2334455"
-- "112.233446"
-- >>> formatFloat "0"
-- "0.000000"
-- >>> formatFloat "0."
-- "0.000000"
-- >>> formatFloat "0.0"
-- "0.000000"
formatFloat :: String -> String
formatFloat s =
    case splitOn "." s of
         [ a, "" ] -> showFFloat (Just 6) (read a :: Double) ""
         _ -> showFFloat (Just 6) (read s :: Double) ""

-- | Round trip from rational to double and back to rational.
-- 
-- >>> roundTripLatLngAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- (-33.650733,147.560367,alt=214)
roundTripLatLngAlt :: (Latitude, Longitude, Altitude)
                   -> (Double, Double, Altitude)
roundTripLatLngAlt (Latitude lat, Longitude lng, alt) =
    let lat' = read $ formatFloat $ show (fromRational lat :: Double)
        lng' = read $ formatFloat $ show (fromRational lng :: Double)
    in (lat', lng', alt)

-- | Comma-separated fields in the order lat,lng,alt.
showLatLngAlt :: (Latitude, Longitude, Altitude) -> String
showLatLngAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lat :: Double)
            , ","
            , formatFloat $ show (fromRational lng :: Double)
            , ","
            , show alt
            ]

-- | Comma-separated fields in the order lng,lat,alt.
showLngLatAlt :: (Latitude, Longitude, Altitude) -> String
showLngLatAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lng :: Double)
            , ","
            , formatFloat $ show (fromRational lat :: Double)
            , ","
            , show alt
            ]

-- | Parse comma-separated triples of lng,lat,alt, each triple separated by
-- spaces.
--
-- >>> parseLngLatAlt "147.932050,-33.361600,237 147.932050,-33.361600,238"
-- [LLA {llaLat = lat=-33.36160000, llaLng = lng=147.93205000, llaAltGps = alt=237},LLA {llaLat = lat=-33.36160000, llaLng = lng=147.93205000, llaAltGps = alt=238}]
parseLngLatAlt :: String -> [LLA]
parseLngLatAlt s =
    case P.parse pFixes "(stdin)" s of
         Left _ -> []
         Right xs ->
             (\(lat, lng, alt) ->
                 LLA
                     (Latitude lat)
                     (Longitude lng)
                     (Altitude alt)) <$> xs

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XNamedFieldPuns
-- >>> import Language.Haskell.TH
-- >>> import Language.Haskell.TH.Syntax (lift)
-- >>> import Flight.Kml
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = lift =<< runIO readStr
-- :}
-- 
-- >>> kml = $(embedStr (readFile "./test-suite-doctest/Phil de Joux.20120114-082221.21437.40.kml"))
-- 

-- $kml
-- #kml#
-- Here's an example of a tracklog dump from the last day of the Hang Gliding
-- Pre-Worlds Forbes 2012. The flight instrument is
-- a <http://www.flytec.com/6030.html Flytec 6030>. The pilot with
-- <http://civlrankings.fai.org/FL.aspx?a=309&person_id=21437 CIVL ID 21437> is
-- me, the author of this package, Phil de Joux.
-- 
-- @
-- \<?xml version="1.0" encoding=\"UTF-8\"?\>
-- \<Document\>
-- \<open\>1\</open\>
--   \<Folder\>
--     \<Metadata src=\"GpsDump\" v="4.66" type="trip"/\>
--     \<open\>1\</open\>
--     \<name\>Trip\</nam\e>
--     \<description\>\<![CDATA[Tracklog from GpsDump competition mode
-- \<pre\>
-- Flight statistics
-- Date                 2012-01-14
-- Start/finish         02:12:55 - 05:51:18
-- Duration             3 : 38 : 23
-- Max./min. height     2392 / 214 m
-- Max. mean/top speed  72 km/h / 82 km/h
-- Max/min climb rate   4.35 / -4.23 m/s over 60s
-- Total distance       166.89 km
-- \</pre\>]]\>
--     \</description\>
-- ...
--     \<Placemark\>
--       \<Metadata src=\"GpsDump\" v="4.66" type="track"\>
--         \<FsInfo time_of_first_point="2012-01-14T02:12:55Z"
--                 civl_pilot_id="21437" comp_pilot_id="40"
--                 instrument="6030 SN06451 SW3.30"
--                 downloaded="2012-01-14T08:22:21Z"
--                 hash="61168B84FE0DAC55F3D65EFBA888B08F72834DDF"\>
--           \<SecondsFromTimeOfFirstPoint\>
-- 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48
-- ...
-- 13061 13063 13065 13067 13069 13071 13073 13075 13077 13079 13081 13083 13085 13087 13089 13091 13093 13095 13097 13099 13101 13103
--           \</SecondsFromTimeOfFirstPoint\>
--           \<PressureAltitude\>
-- 239 240 240 239 239 239 239 239 239 240 239 240 239 239 240 239 240 240 240 240 239 239 240 240 240 
-- ...
-- 237 237 237 237 237 237 237 238 238 237 238 237 237 238 237 237 238 238 237 238 237 238
--           \</PressureAltitude\>
--         \</FsInfo\>
--       \</Metadata\>
--       \<name\>Tracklog\</name\>
--       \<LineString\>
--         \<altitudeMode\>absolute\</altitudeMode\>
--         \<coordinates\>
-- 147.932050,-33.361600,237 147.932050,-33.361600,238 147.932050,-33.361600,238 147.932050,-33.361600,238 147.932067,-33.361600,238 
-- ...
-- 147.560367,-33.650733,215 147.560367,-33.650733,215 147.560367,-33.650733,214 147.560367,-33.650733,214 147.560367,-33.650733,214 
-- 147.560367,-33.650733,214 147.560367,-33.650733,214
--         \</coordinates\>
--       \</LineString\>
--     \</Placemark\>
--   \</Folder\>
-- \</Document\>
-- 
-- @

-- $use
-- Working with the <#kml KML tracklog dump> from the tracklog file "__@Phil de Joux.20120114-082221.21437.40.kml@__".
--
-- >>> Right mf@(MarkedFixes{mark0, fixes}) <- parse kml
-- >>> mark0
-- 2012-01-14 02:12:55 UTC
-- >>> length fixes
-- 6547
-- >>> head fixes
-- Fix {fixMark = sec=0, fix = LLA {llaLat = lat=-33.36160000, llaLng = lng=147.93205000, llaAltGps = alt=237}, fixAltBaro = Just alt=239}
-- >>> last fixes
-- Fix {fixMark = sec=13103, fix = LLA {llaLat = lat=-33.65073300, llaLng = lng=147.56036700, llaAltGps = alt=214}, fixAltBaro = Just alt=238}
--
-- The length and range of the tracklog.
--
-- >>> fixesLength mf
-- 6547
-- >>> fixesSecondsRange mf
-- Just (sec=0,sec=13103)
-- >>> fixesUTCTimeRange mf
-- Just (2012-01-14 02:12:55 UTC,2012-01-14 05:51:18 UTC)
--
-- Showing the fixes in the tracklog.
--
-- >>> showFixesLength mf
-- "6547"
-- >>> showFixesSecondsRange mf
-- "(sec=0,sec=13103)"
-- >>> showFixesUTCTimeRange mf
-- "(2012-01-14 02:12:55 UTC,2012-01-14 05:51:18 UTC)"
--
-- Showing a single fix.
--
-- >>> let a = head fixes
-- >>> let z = last fixes
-- >>> let lla = (lat . fix $ a, lng . fix $ a, altGps . fix $ a)
-- >>> showLatLngAlt lla
-- "-33.361600,147.932050,237"
-- >>> showLngLatAlt lla
-- "147.932050,-33.361600,237"
-- >>> showTimeAlt a
-- "(0s,237m)"
-- >>> showTimeAlt z
-- "(13103s,214m)"
