{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Map.Leaflet
    ( Map(..)
    , TileLayer(..)
    , Marker(..)
    , map
    , mapSetView
    , mapInvalidateSize
    , tileLayer
    , tileLayerAddToMap
    , marker
    , markerAddToMap
    , markerPopup
    , circle
    , circleAddToMap
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement(..))
import GHCJS.DOM.Types (Element(..), toElement, toJSString)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Map = Map { unMap :: JSVal }
newtype TileLayer = TileLayer { unTileLayer :: JSVal }
newtype Marker = Marker { unMarker :: JSVal }
newtype Circle = Circle { unCircle :: JSVal }

foreign import javascript unsafe
    "L['map']($1)"
    map_ :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['setView']([$2, $3], $4)"
    mapSetView_ :: JSVal -> Double -> Double -> Int -> IO ()

foreign import javascript unsafe
    "$1['invalidateSize']()"
    mapInvalidateSize_ :: JSVal -> IO () 

foreign import javascript unsafe
    "L['tileLayer']($1, { maxZoom: $2, attribution: $3})"
    tileLayer_ :: JSString -> Int -> JSString -> IO JSVal 

foreign import javascript unsafe
    "$1['addTo']($2)"
    tileLayerAddToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "L['marker']([$1, $2])"
    marker_ :: Double -> Double -> IO JSVal

foreign import javascript unsafe
    "$1['addTo']($2)"
    markerAddToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "$1['bindPopup']($2)"
    markerPopup_ :: JSVal -> JSString -> IO ()

foreign import javascript unsafe
    "L['circle']([$1, $2], {radius: $3})"
    circle_ :: Double -> Double -> Double -> IO JSVal

foreign import javascript unsafe
    "$1['addTo']($2)"
    circleAddToMap_ :: JSVal -> JSVal -> IO ()

map :: IsElement e => e -> IO Map
map e = do
    lmap <- map_ $ unElement $ toElement e
    return $ Map lmap

mapSetView :: Map -> (Double, Double) -> Int -> IO ()
mapSetView lm (lat, lng) zoom = mapSetView_ (unMap lm) lat lng zoom

mapInvalidateSize :: Map -> IO ()
mapInvalidateSize lmap = mapInvalidateSize_ (unMap lmap)

tileLayer :: String -> Int -> String -> IO TileLayer
tileLayer src maxZoom attribution = do
    layer <- tileLayer_ (toJSString src) maxZoom (toJSString attribution)
    return $ TileLayer layer

tileLayerAddToMap :: TileLayer -> Map -> IO ()
tileLayerAddToMap layer lmap = tileLayerAddToMap_ (unTileLayer layer) (unMap lmap)

marker :: (Double, Double) -> IO Marker
marker (lat, lng) = do
    marker <- marker_ lat lng
    return $ Marker marker

markerAddToMap :: Marker -> Map -> IO ()
markerAddToMap marker lmap = markerAddToMap_ (unMarker marker) (unMap lmap)

circle :: (Double, Double) -> Double -> IO Circle
circle (lat, lng) radius = do
    circle <- circle_ lat lng radius
    return $ Circle circle

circleAddToMap :: Circle -> Map -> IO ()
circleAddToMap marker lmap = circleAddToMap_ (unCircle marker) (unMap lmap)

markerPopup :: Marker -> String -> IO ()
markerPopup marker msg = markerPopup_ (unMarker marker) (toJSString msg)
