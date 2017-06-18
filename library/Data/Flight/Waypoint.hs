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
    , (/>)
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
import Data.Flight.Types
    ( Latitude
    , Longitude
    , Turnpoint(..)
    , Task(..)
    )

getTask :: ArrowXml a => a XmlTree Task
getTask =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& listA getDefn
    >>> arr (uncurry Task)
    where
        getDefn =
            getChildren
            >>> hasName "FsTaskDefinition"
            /> hasName "FsTurnpoint"
            >>> getAttrValue "id"
            &&& getAttrValue "lat"
            &&& getAttrValue "lon"
            &&& getAttrValue "radius"
            >>> arr (\(name, (lat, (lng, rad))) -> (Turnpoint name lat lng rad))

parse :: String -> IO (Either String [ Task ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTask
    return $ Right xs
