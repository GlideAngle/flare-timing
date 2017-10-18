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
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Data.List (concatMap, nub)
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

import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone.Raw (RawZone(..))
import Flight.Comp
    (Task(..), SpeedSection, StartGate(..), OpenClose(..))

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
    >>> arr mkTask
    where
        getDefn =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> getSpeedSection
            >>. take 1
            &&& listA getTps
            &&& listA getOpenClose
            &&& listA getGates

        getSpeedSection =
            (getAttrValue "ss" &&& getAttrValue "es")
            >. parseSpeedSection

        getOpenClose =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> getAttrValue "open"
            &&& getAttrValue "close"
            >>> arr parseOpenClose

        getTps =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> getAttrValue "id"
            &&& getAttrValue "lat"
            &&& getAttrValue "lon"
            &&& getAttrValue "radius"
            >>> arr (\(name, (lat', (lng', rad))) -> (name, lat', lng', rad))
            >>. concatMap parseZone

        getGates =
            getChildren
            >>> hasName "FsStartGate"
            >>> getAttrValue "open"
            >>> arr parseStartGate

        mkTask (name, (section, (zs, (ts, gates)))) =
            Task name zs section ts'' gates
            where
                -- NOTE: If all time zones are the same then collapse.
                ts' = nub ts
                ts'' = if length ts' == 1 then ts' else ts

parseTasks :: String -> IO (Either String [Task])
parseTasks contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTask
    return $ Right xs

pRadius :: GenParser Char st Integer
pRadius = pNat <?> "No radius"

parseUtcTime :: String -> UTCTime
parseUtcTime =
    -- NOTE: %F is %Y-%m-%d, %T is %H:%M:%S and %z is -HHMM or -HH:MM
    parseTimeOrError False defaultTimeLocale "%FT%T%z"

parseStartGate :: String -> StartGate
parseStartGate =
    StartGate . parseUtcTime

parseOpenClose :: (String, String) -> OpenClose
parseOpenClose (o, c) =
    OpenClose (parseUtcTime o) (parseUtcTime c)

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

parseZone :: (String, String, String, String) -> [RawZone]
parseZone (name, tpLat, tpLng, tpRadius) =
    case (latlng, rad) of
        (Right [ lat', lng' ], Right rad') ->
            [ RawZone name (RawLat lat') (RawLng lng') rad' ]

        _ ->
            []
    where
        latlng =
            sequence [ P.parse (pRat "No latitude") "" tpLat
                     , P.parse (pRat "No longitude") "" tpLng
                     ]

        rad =
            P.parse pRadius "" tpRadius
