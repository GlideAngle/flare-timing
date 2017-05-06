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
import Text.XML.HXT.XPath (getXPathTreesInDoc)

parse :: String -> IO (Either String String)
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    contents <- runX $
            doc
            >>>
            getXPathTreesInDoc "//Placemark[Metadata[@type='track']]"
            />
            hasName "LineString"
    return $ Right $ show contents
