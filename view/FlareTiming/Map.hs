{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype LeafletMap =
    LeafletMap { unLeafletMap :: JSRef LeafletMap }

newtype LeafletTileLayer =
    LeafletTileLayer { unLeafletTileLayer :: JSRef LeafletTileLayer }

newtype LeafletMarker =
    LeafletMarker { unLeafletMarker :: JSRef LeafletMarker }

foreign import javascript unsafe
    "L['map']($1)"
    leafletMap_ :: JSRef Element
                -> IO (JSRef LeafletMap)

foreign import javascript unsafe
    "$1['setView']([$2, $3], $4)"
    leafletMapSetView_ :: JSRef LeafletMap
                       -> Double
                       -> Double
                       -> Int
                       -> IO ()

foreign import javascript unsafe
    "L['marker']([$1, $2])"
    leafletMarker_ :: Double
                   -> Double
                   -> IO (JSRef LeafletMarker)

foreign import javascript unsafe
    "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
    leafletTileLayer_ :: JSString
                      -> Int
                      -> JSString
                      -> IO (JSRef LeafletTileLayer)

foreign import javascript unsafe
    "$1['addTo']($2)"
    leafletTileLayerAddToMap_ :: JSRef LeafletTileLayer
                              -> JSRef LeafletMap
                              -> IO ()

foreign import javascript unsafe
    "$1['addTo']($2)"
    leafletMarkerAddToMap_ :: JSRef LeafletMarker
                           -> JSRef LeafletMap
                           -> IO ()

foreign import javascript unsafe
    "$1.invalidateSize()"
    leafletMapInvalidateSize_ :: JSRef LeafletMap
                              -> IO ()

foreign import javascript unsafe
    "$1.bindPopup($2).openPopup()"
    leafletMarkerPopup_ :: JSRef LeafletMarker
                        -> JSString
                        -> IO ()

leafletMap :: IsElement e => e -> IO LeafletMap
leafletMap e = do
    lmap <- leafletMap_ $ unElement $ toElement e
    return $ LeafletMap lmap

leafletMapSetView :: LeafletMap -> (Double, Double) -> Int -> IO ()
leafletMapSetView lm (lat, lng) zoom =
    leafletMapSetView_ (unLeafletMap lm) lat lng zoom

leafletMarker :: (Double, Double) -> IO LeafletMarker
leafletMarker (lat, lng) = do
    marker <- leafletMarker_ lat lng
    return $ LeafletMarker marker

leafletTileLayer :: String -> Int -> String -> IO LeafletTileLayer
leafletTileLayer src maxZoom attribution = do
    layer <- leafletTileLayer_ (toJSString src) maxZoom (toJSString attribution)
    return $ LeafletTileLayer layer

leafletMarkerAddToMap :: LeafletMarker -> LeafletMap -> IO ()
leafletMarkerAddToMap marker lmap =
    leafletMarkerAddToMap_ (unLeafletMarker marker) (unLeafletMap lmap)

leafletTileLayerAddToMap :: LeafletTileLayer -> LeafletMap -> IO ()
leafletTileLayerAddToMap layer lmap = 
    leafletTileLayerAddToMap_ (unLeafletTileLayer layer) (unLeafletMap lmap)

leafletMarkerPopup :: LeafletMarker -> String -> IO ()
leafletMarkerPopup marker msg =
    leafletMarkerPopup_ (unLeafletMarker marker) (toJSString msg)

leafletMapInvalidateSize :: LeafletMap -> IO ()
leafletMapInvalidateSize lmap =
    leafletMapInvalidateSize_ (unLeafletMap lmap)

map :: MonadWidget t m => m ()
map = do
    postBuild <- delay 0 =<< getPostBuild
    (e, _) <- elAttr' "div" ("style" =: "height: 600px;") $ return ()
    rec performEvent_ $ fmap (\_ -> liftIO $ leafletMapInvalidateSize lmap) postBuild
        lmap <- liftIO $ do
            lmap <- leafletMap (_element_raw e)
            leafletMapSetView lmap (-33.36137, 147.93207) 12
            layer <-
                leafletTileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17
                    "Map data: &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>, <a href='http://viewfinderpanoramas.org'>SRTM</a> | Map style: &copy; <a href='https://opentopomap.org'>OpenTopoMap</a> (<a href='https://creativecommons.org/licenses/by-sa/3.0/'>CC-BY-SA</a>)"

            leafletTileLayerAddToMap layer lmap

            mark <- leafletMarker (-33.36137, 147.9320)

            leafletMarkerAddToMap mark lmap

            leafletMarkerPopup mark "FORBES"

            return lmap

    return ()

