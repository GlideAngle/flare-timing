{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module FlareTiming.Map (map) where

import Prelude hiding (map)
import Reflex.Dom
    ( MonadWidget
    , (=:)
    , elAttr'
    , getPostBuild
    , performEvent_
    , _element_raw
    )
import Reflex.Dom.Time (delay)
import Control.Monad.IO.Class (liftIO)

import qualified FlareTiming.Map.Leaflet as L
    ( Map(..)
    , TileLayer(..)
    , Marker(..)
    , map
    , mapSetView
    , tileLayer
    , tileLayerAddToMap
    , marker
    , markerAddToMap
    , markerPopup
    , mapInvalidateSize
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
    rec performEvent_ $ fmap (\_ -> liftIO $ L.mapInvalidateSize lmap) postBuild
        lmap <- liftIO $ do
            lmap <- L.map (_element_raw e)
            L.mapSetView lmap (-33.36137, 147.93207) 12
            layer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                L.tileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17
                    attribution

            L.tileLayerAddToMap layer lmap
            mark <- L.marker (-33.36137, 147.9320)
            L.markerAddToMap mark lmap
            L.markerPopup mark "FORBES"
            return lmap

    return ()

