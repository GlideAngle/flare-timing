{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Data.Waypoint
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides parsing the fsdb format for competitors, tasks and results.
-}
module Data.Flight.Waypoint
    ( Task
    , Latitude
    , Longitude
    , parse
    ) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (&&&)
    , (>>>)
    , (>>.)
    , (>.)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , listA
    , arr
    , deep
    )
import Data.List (concatMap)
import Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
    ( GenParser
    , (<?>)
    , char
    , option
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)

import Data.Flight.Types
    ( Latitude
    , Longitude
    , Turnpoint(..)
    , Task(..)
    , SpeedSection
    )

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Rational
pFloat = parsecMap toRational $ P.float lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

getTask :: ArrowXml a => a XmlTree Task
getTask =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& getDefn
    >>> arr (\(name, (speedSection, tps)) -> Task name speedSection tps)
    where
        getDefn =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> getSpeedSection
            >>. take 1
            &&& listA getTps

        getSpeedSection =
            (getAttrValue "ss" &&& getAttrValue "es")
            >. parseSpeedSection

        getTps =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> getAttrValue "id"
            &&& getAttrValue "lat"
            &&& getAttrValue "lon"
            &&& getAttrValue "radius"
            >>> arr (\(name, (lat, (lng, rad))) -> (name, lat, lng, rad))
            >>. concatMap parseTurnpoint

parse :: String -> IO (Either String [ Task ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTask
    return $ Right xs

pRat :: String -> GenParser Char st Rational
pRat errMsg = do
    sign <- option id $ const negate <$> char '-'
    x <- pFloat <?> errMsg
    return $ sign x

pRadius :: GenParser Char st Integer
pRadius = pNat <?> "No radius"

parseSpeedSection :: [(String, String)] -> SpeedSection
parseSpeedSection [] = Nothing
parseSpeedSection ((ss, es) : _) =
    case speedSection of
         Right [ ss', es' ] -> Just (ss', es')
         _ -> Nothing
    where
        speedSection =
            sequence [ P.parse pNat "" ss
                     , P.parse pNat "" es
                     ]

parseTurnpoint :: (String, String, String, String) -> [ Turnpoint ]
parseTurnpoint (name, lat, lng, radius) =
    case (latlng, rad) of
        (Right [ lat', lng' ], Right rad') ->  [ Turnpoint name lat' lng' rad' ]
        _ -> []
    where
        latlng =
            sequence [ P.parse (pRat "No latitude") "" lat
                     , P.parse (pRat "No longitude") "" lng
                     ]

        rad =
            P.parse pRadius "" radius
