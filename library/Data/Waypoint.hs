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
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.XPath (getXPathTreesInDoc, getXPathTrees)

data Fix = Fix String String String deriving Show

parse :: String -> IO (Either String [ Fix ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents

    seconds <- runX $ doc
        >>> getXPathTreesInDoc "//Placemark[Metadata[@type='track']]"
        >>> getXPathTrees "//SecondsFromTimeOfFirstPoint"
        /> getText

    baro <- runX $ doc
        >>> getXPathTreesInDoc "//Placemark[Metadata[@type='track']]"
        >>> getXPathTrees "//PressureAltitude"
        /> getText

    tracklog <- runX $ doc
        >>> getXPathTreesInDoc "//Placemark[Metadata[@type='track']]"
        >>> getXPathTrees "//LineString/coordinates"
        /> getText

    return $ Right $ zipWith3 Fix seconds baro tracklog
