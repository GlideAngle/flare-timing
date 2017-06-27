{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Map.Leaflet
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
    ) where

import Prelude hiding (map)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement(..))
import GHCJS.DOM.Types (Element(..), toElement, toJSString)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Map = Map { unMap :: JSVal }
newtype TileLayer = TileLayer { unTileLayer :: JSVal }
newtype Marker = Marker { unMarker :: JSVal }

foreign import javascript unsafe
    "L['map']($1)"
    map_ :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['setView']([$2, $3], $4)"
    mapSetView_ :: JSVal -> Double -> Double -> Int -> IO ()

foreign import javascript unsafe
    "L['marker']([$1, $2])"
    marker_ :: Double -> Double -> IO JSVal

foreign import javascript unsafe
    "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
    tileLayer_ :: JSString -> Int -> JSString -> IO JSVal 

foreign import javascript unsafe
    "$1['addTo']($2)"
    tileLayerAddToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "$1['addTo']($2)"
    markerAddToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "$1.invalidateSize()"
    mapInvalidateSize_ :: JSVal -> IO () 

foreign import javascript unsafe
    "$1.bindPopup($2).openPopup()"
    markerPopup_ :: JSVal -> JSString -> IO ()

map :: IsElement e => e -> IO Map
map e = do
    lmap <- map_ $ unElement $ toElement e
    return $ Map lmap

mapSetView :: Map -> (Double, Double) -> Int -> IO ()
mapSetView lm (lat, lng) zoom = mapSetView_ (unMap lm) lat lng zoom

marker :: (Double, Double) -> IO Marker
marker (lat, lng) = do
    marker <- marker_ lat lng
    return $ Marker marker

tileLayer :: String -> Int -> String -> IO TileLayer
tileLayer src maxZoom attribution = do
    layer <- tileLayer_ (toJSString src) maxZoom (toJSString attribution)
    return $ TileLayer layer

markerAddToMap :: Marker -> Map -> IO ()
markerAddToMap marker lmap = markerAddToMap_ (unMarker marker) (unMap lmap)

tileLayerAddToMap :: TileLayer -> Map -> IO ()
tileLayerAddToMap layer lmap = tileLayerAddToMap_ (unTileLayer layer) (unMap lmap)

markerPopup :: Marker -> String -> IO ()
markerPopup marker msg = markerPopup_ (unMarker marker) (toJSString msg)

mapInvalidateSize :: Map -> IO ()
mapInvalidateSize lmap = mapInvalidateSize_ (unMap lmap)
