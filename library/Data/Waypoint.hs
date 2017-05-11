{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

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
    , timeToParse
    , baroToParse
    , trackToParse
    , parseTime
    , parseBaro
    , parseTrack
    ) where

import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.DOM.TypeDefs (XmlTree, XNode)
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
    , arr
    )
import Data.List (concatMap)
import Text.Parsec.Token as P
import Text.Parsec.Char (endOfLine, anyChar)
import Text.ParserCombinators.Parsec
    ( GenParser
    , char
    , many
    , many1
    , manyTill
    , eof
    , option
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)
import Text.RawString.QQ (r)

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Double
pFloat = P.float lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

data Fix = Fix String String String deriving Show

isMetadata :: ArrowXml a => a (NTree XNode) XmlTree
isMetadata =
    getChildren
    >>> hasName "Metadata"
    >>> hasAttrValue "type" (== "track")

getTrack :: ArrowXml a => a XmlTree XmlTree
getTrack =
    getChildren
    >>> hasName "Document"
    /> hasName "Folder"
    /> hasName "Placemark"
    >>> filterA isMetadata
    >>. take 1

getFsInfo :: ArrowXml a => a XmlTree XmlTree
getFsInfo =
    getChildren
    >>> hasName "Metadata"
    /> hasName "FsInfo"
    >>. take 1

getFix :: ArrowXml a => a XmlTree [ Fix ]
getFix =
    getTrack
    >>> (listA getCoord &&& (getFsInfo >>> (listA getTime &&& listA getBaro)))
    >>> arr (\(c, (a, b)) -> zipWith3 Fix a b c)

getTime :: ArrowXml a => a XmlTree String
getTime =
    getChildren
    >>> hasName "SecondsFromTimeOfFirstPoint"
    /> getText
    >>. concatMap parseTime

getBaro :: ArrowXml a => a XmlTree String
getBaro =
    getChildren
    >>> hasName "PressureAltitude"
    /> getText
    >>. concatMap parseBaro

getCoord :: ArrowXml a => a XmlTree String
getCoord =
    getChildren
    >>> hasName "LineString"
    /> hasName "coordinates"
    /> getText
    >>. concatMap parseTrack

parse :: String -> IO (Either String [ Fix ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getFix
    return $ Right $ concat xs

pTimes :: GenParser Char st [ Integer ]
pTimes = do
    xs <- many pTime
    _ <- eof
    return $ concat xs

pTime :: GenParser Char st [ Integer ]
pTime = manyTill anyChar endOfLine *> many1 pNat

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

pTracks :: GenParser Char st [ (Double, Double, Integer) ]
pTracks = do
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

parseTrack :: String -> [ String ]
parseTrack s =
    case P.parse pTracks "(stdin)" s of
         Left msg -> [ show msg ]
         Right xs -> show <$> xs

timeToParse :: String
timeToParse = [r|
0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120
125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 231 236 241 246 
12519 12524 12529 12534 12539 12544 12549 12554 12559 12564 12569 12574 12579 12584 12589 12594 12599 12604 12609 12614 12619 12624 12629 12634 12639 
12644 12649 12654 12659 12664 12669 12674 12679 12684 12689
              |]

baroToParse :: String
baroToParse = [r|
221 221 221 221 221 221 221 221 221 221 221 221 221 221 221 222 222 222 222 222 222 222 222 222 222 
222 222 221 225 232 246 262 268 274 279 290 305 321 340 356 372 389 401 409 418 422 422 430 428 442 
399 397 393 384 376 368 360 350 343 340 330 319 313 313 311 311 311 311 311 311 311 312 312 312 312 
312 312 312 312 312 312 312 312 312 312
              |]

trackToParse :: String
trackToParse = [r|
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 147.932417,-33.360950,241 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 
147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277 147.932183,-33.708533,277
              |]
