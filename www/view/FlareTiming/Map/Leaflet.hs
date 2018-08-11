{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Map.Leaflet
    ( Map(..)
    , TileLayer(..)
    , Marker(..)
    , Circle(..)
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
    , polyline 
    , polylineAddToMap
    , circleBounds
    , polylineBounds
    , extendBounds
    , fitBounds
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement(..))
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Map = Map { unMap :: JSVal }
newtype TileLayer = TileLayer { unTileLayer :: JSVal }
newtype Marker = Marker { unMarker :: JSVal }
newtype Circle = Circle { unCircle :: JSVal }
newtype Polyline = Polyline { unPolyline :: JSVal }
newtype LatLngBounds = LatLngBounds { unLatLngBounds :: JSVal }

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
    "L['tileLayer']($1, { maxZoom: $2, opacity: 0.6})"
    tileLayer_ :: JSString -> Int -> IO JSVal 

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

foreign import javascript unsafe
    "L['polyline']($1, {color: 'red'})"
    polyline_ :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['addTo']($2)"
    polylineAddToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "$1['getBounds']()"
    getBounds_ :: JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['extend']($2)"
    extendBounds_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['fitBounds']($2)"
    fitBounds_ :: JSVal -> JSVal -> IO ()

map :: IsElement e => e -> IO Map
map e = do
    lmap <- map_ $ unElement $ toElement e
    return $ Map lmap

mapSetView :: Map -> (Double, Double) -> Int -> IO ()
mapSetView lm (lat, lng) zoom = mapSetView_ (unMap lm) lat lng zoom

mapInvalidateSize :: Map -> IO ()
mapInvalidateSize lmap = mapInvalidateSize_ (unMap lmap)

tileLayer :: String -> Int -> IO TileLayer
tileLayer src maxZoom = do
    layer <- tileLayer_ (toJSString src) maxZoom
    return $ TileLayer layer

tileLayerAddToMap :: TileLayer -> Map -> IO ()
tileLayerAddToMap x lmap = tileLayerAddToMap_ (unTileLayer x) (unMap lmap)

marker :: (Double, Double) -> IO Marker
marker (lat, lng) = do
    marker <- marker_ lat lng
    return $ Marker marker

markerAddToMap :: Marker -> Map -> IO ()
markerAddToMap x lmap = markerAddToMap_ (unMarker x) (unMap lmap)

markerPopup :: Marker -> String -> IO ()
markerPopup x msg = markerPopup_ (unMarker x) (toJSString msg)

circle :: (Double, Double) -> Double -> IO Circle
circle (lat, lng) radius = do
    circle <- circle_ lat lng radius
    return $ Circle circle

circleAddToMap :: Circle -> Map -> IO ()
circleAddToMap x lmap = circleAddToMap_ (unCircle x) (unMap lmap)

polyline :: [(Double, Double)] -> IO Polyline
polyline xs = do
    ys <- toJSVal $ (\ (lat, lng) -> [ lat, lng ]) <$> xs
    polyline <- polyline_ ys
    return $ Polyline polyline

polylineAddToMap :: Polyline -> Map -> IO ()
polylineAddToMap x lmap = polylineAddToMap_ (unPolyline x) (unMap lmap)

circleBounds :: Circle -> IO LatLngBounds
circleBounds x = do
    bounds <- getBounds_ (unCircle x)
    return $ LatLngBounds bounds

polylineBounds :: Polyline -> IO LatLngBounds
polylineBounds x = do
    bounds <- getBounds_ (unPolyline x)
    return $ LatLngBounds bounds

extendBounds :: LatLngBounds -> LatLngBounds -> IO LatLngBounds
extendBounds x y = do
    bounds <- extendBounds_ (unLatLngBounds x) (unLatLngBounds y)
    return $ LatLngBounds bounds

fitBounds :: Map -> LatLngBounds -> IO ()
fitBounds lm bounds = fitBounds_ (unMap lm) (unLatLngBounds bounds)
