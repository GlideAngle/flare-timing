{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Waypoint
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the KML format for waypoint fixes.
-}
module Data.Waypoint
    (
    parse
    , parseTime
    , parseBaro
    , parseCoord
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
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , hasAttrValue
    , filterA
    , listA
    , unlistA
    , arr
    )
import Data.List (concatMap)
import Text.Parsec.Token as P
import Text.Parsec.Char (endOfLine, anyChar, spaces)
import Text.ParserCombinators.Parsec
    ( GenParser
    , char
    , many
    , many1
    , manyTill
    , eof
    , option
    , sepBy
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Double
pFloat = P.float lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

data Fix = Fix String String String deriving Show

getFix :: ArrowXml a => a XmlTree Fix
getFix =
    getTrack
    >>> (listA getCoord &&& (getFsInfo >>> (listA getTime &&& listA getBaro)))
    >>> arr (\(c, (a, b)) -> zipWith3 Fix a b c)
    >>> unlistA
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

        getFsInfo =
            getChildren
            >>> hasName "Metadata"
            /> hasName "FsInfo"
            >>. take 1

        getTime =
            getChildren
            >>> hasName "SecondsFromTimeOfFirstPoint"
            /> getText
            >>. concatMap parseTime

        getBaro =
            getChildren
            >>> hasName "PressureAltitude"
            /> getText
            >>. concatMap parseBaro

        getCoord =
            getChildren
            >>> hasName "LineString"
            /> hasName "coordinates"
            /> getText
            >>. concatMap parseCoord

parse :: String -> IO (Either String [ Fix ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getFix
    return $ Right xs

pTimes :: GenParser Char st [ Integer ]
pTimes = do
    _ <- spaces
    xs <- pNat `sepBy` spaces
    _ <- eof
    return xs

parseTime :: String -> [ String ]
parseTime s =
    case P.parse pTimes "(stdin)" s of
         Left msg -> [ show msg ]
         Right xs -> show <$> xs

pBaros :: GenParser Char st [ Integer ]
pBaros = do
    xs <- many pBaro
    _ <- eof
    return $ concat xs

pBaro :: GenParser Char st [ Integer ]
pBaro = manyTill anyChar endOfLine *> many1 pNat

parseBaro :: String -> [ String ]
parseBaro s =
    case P.parse pBaros "(stdin)" s of
         Left msg -> [ show msg ]
         Right xs -> show <$> xs

pCoords :: GenParser Char st [ (Double, Double, Integer) ]
pCoords = do
    xs <- many pTrack
    _ <- eof
    return $ concat xs

pFix :: GenParser Char st (Double, Double, Integer)
pFix = do
    latSign <- option id $ const negate <$> char '-'
    lat <- pFloat
    _ <- char ','
    lngSign <- option id $ const negate <$> char '-'
    lng <- pFloat
    _ <- char ','
    altSign <- option id $ const negate <$> char '-'
    alt <- pNat
    return (latSign lat, lngSign lng, altSign alt)

pTrack :: GenParser Char st [ (Double, Double, Integer) ]
pTrack = do
    _ <- manyTill anyChar endOfLine
    many1 pFix

parseCoord :: String -> [ String ]
parseCoord s =
    case P.parse pCoords "(stdin)" s of
         Left msg -> [ show msg ]
         Right xs -> show <$> xs
