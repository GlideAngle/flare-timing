{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Flight.Kml
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the KML format for waypoint fixes.
-}
module Flight.Kml
    ( Fix
    , MarkedFixes(..)
    , LLA
    , T.LatLngAlt(..)
    , T.FixMark(..)
    , Seconds(..)
    , Latitude(..)
    , Longitude(..)
    , Altitude(..)
    , mkPosition
    , parse
    , parseTimeOffsets
    , parseBaroMarks
    , parseLngLatAlt
    , showLatLngAlt
    , showLngLatAlt
    , roundTripLatLngAlt
    , formatFloat
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
parseUtcTime s = do
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

parseTimeOffsets :: String -> [Seconds]
parseTimeOffsets s =
    case P.parse pNats "(stdin)" s of
        Left _ -> []
        Right xs -> Seconds <$> xs

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

formatFloat :: String -> String
formatFloat s =
    -- NOTE: Avoid "0." because ...
    --    *Main Data.Waypoint> (read "0." :: Double)
    --    *** Exception: Prelude.read: no parse
    --    *Main Data.Waypoint> (read "0.0" :: Double)
    --    0.0
    case splitOn "." s of
         [ a, "" ] -> showFFloat (Just 6) (read a :: Double) ""
         _ -> showFFloat (Just 6) (read s :: Double) ""

-- | Round trip from rational to double and back to rational.
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
