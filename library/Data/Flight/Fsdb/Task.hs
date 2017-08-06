{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Flight.Fsdb.Task (parseTasks) where

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

import Data.Flight.Comp
    ( Latitude(..)
    , Longitude(..)
    , Zone(..)
    , Task(..)
    , SpeedSection
    )

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Rational
pFloat = parsecMap toRational $ P.float lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

pRat :: String -> GenParser Char st Rational
pRat errMsg = do
    sign <- option id $ const negate <$> char '-'
    x <- pFloat <?> errMsg
    return $ sign x

getTask :: ArrowXml a => a XmlTree Task
getTask =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& getDefn
    >>> arr (\(name, (section, tps)) -> Task name section tps)
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
            >>> hasName "FsZone"
            >>> getAttrValue "id"
            &&& getAttrValue "lat"
            &&& getAttrValue "lon"
            &&& getAttrValue "radius"
            >>> arr (\(name, (lat', (lng', rad))) -> (name, lat', lng', rad))
            >>. concatMap parseZone

parseTasks :: String -> IO (Either String [Task])
parseTasks contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTask
    return $ Right xs

pRadius :: GenParser Char st Integer
pRadius = pNat <?> "No radius"

parseSpeedSection :: [(String, String)] -> SpeedSection
parseSpeedSection [] = Nothing
parseSpeedSection ((ss, es) : _) =
    case section of
         Right [ ss', es' ] -> Just (ss', es')
         _ -> Nothing
    where
        section =
            sequence [ P.parse pNat "" ss
                     , P.parse pNat "" es
                     ]

parseZone :: (String, String, String, String) -> [Zone]
parseZone (name, tpLat, tpLng, tpRadius) =
    case (latlng, rad) of
        (Right [ lat', lng' ], Right rad') ->
            [ Zone name (Latitude lat') (Longitude lng') rad' ]

        _ ->
            []
    where
        latlng =
            sequence [ P.parse (pRat "No latitude") "" tpLat
                     , P.parse (pRat "No longitude") "" tpLng
                     ]

        rad =
            P.parse pRadius "" tpRadius
