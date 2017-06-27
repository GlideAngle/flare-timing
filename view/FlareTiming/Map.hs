{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Map (map) where

import Prelude hiding (map)
import Control.Monad
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Reflex.Dom
    ( MonadWidget
    , (=:)
    , elAttr'
    , getPostBuild
    , performEvent_
    , _element_raw
    )
import Reflex.Dom.Time (delay)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)
import Data.Monoid
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Element
import GHCJS.DOM.Types
import Control.Monad.IO.Class

import FlareTiming.Map.Leaflet
    ( LeafletMap(..)
    , LeafletTileLayer(..)
    , LeafletMarker(..)
    , leafletMap
    , leafletMapSetView
    , leafletTileLayer
    , leafletTileLayerAddToMap
    , leafletMarker
    , leafletMarkerAddToMap
    , leafletMarkerPopup
    , leafletMapInvalidateSize
    )

attribution :: String
attribution =
    unlines [ "Map data: &copy; <a href='http://www.openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a>, "
            , "<a href='http://viewfinderpanoramas.org' target='_blank'>SRTM</a>"
            , " | Map style: &copy; "
            , "<a href='https://opentopomap.org' target='_blank'>OpenTopoMap</a>"
            , " ("
            , "<a href='https://creativecommons.org/licenses/by-sa/3.0/' target='_blank'>CC-BY-SA</a>"
            , ")"
            ]

map :: MonadWidget t m => m ()
map = do
    postBuild <- delay 0 =<< getPostBuild
    (e, _) <- elAttr' "div" ("style" =: "height: 600px;") $ return ()
    rec performEvent_ $ fmap (\_ -> liftIO $ leafletMapInvalidateSize lmap) postBuild
        lmap <- liftIO $ do
            lmap <- leafletMap (_element_raw e)
            leafletMapSetView lmap (-33.36137, 147.93207) 12
            layer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                leafletTileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17
                    attribution

            leafletTileLayerAddToMap layer lmap

            mark <- leafletMarker (-33.36137, 147.9320)

            leafletMarkerAddToMap mark lmap

            leafletMarkerPopup mark "FORBES"

            return lmap

    return ()

