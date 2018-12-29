{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FlareTiming.Map.Leaflet
    ( Map(..)
    , TileLayer(..)
    , Marker(..)
    , Circle(..)
    , LatLngBounds
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
    , panToBounds
    , latLngBounds
    , layerGroup
    , layerGroupAddToMap
    , layersControl
    ) where

import Prelude hiding (map, log)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Element (IsElement)
import GHCJS.DOM.Types (Element(..), toElement, toJSString, toJSVal)

-- SEE: https://gist.github.com/ali-abrar/fa2adbbb7ee64a0295cb
newtype Map = Map { unMap :: JSVal }
newtype LayerGroup = LayerGroup { unLayerGroup :: JSVal }
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
    "L['tileLayer']($1, {maxZoom: $2, opacity: 0.6})"
    tileLayer_ :: JSString -> Int -> IO JSVal

foreign import javascript unsafe
    "$1['addTo']($2)"
    addToMap_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "L.layerGroup($2).addLayer($1)"
    layerGroup_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "L.control.layers(\
    \{ 'Course line (point to point)': $3\
    \, 'Course line (shortest route)': $4\
    \, 'Speed section (course line subset)': $5\
    \, 'Speed section (task waypoints subset)': $6\
    \}\
    \, {'Map': $1}).addTo($2)"
    layersControl_ :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "L['marker']([$1, $2])"
    marker_ :: Double -> Double -> IO JSVal

foreign import javascript unsafe
    "$1['bindPopup']($2)"
    markerPopup_ :: JSVal -> JSString -> IO ()

foreign import javascript unsafe
    "L['circle']([$1, $2], {radius: $3, color: $4, opacity: 0.6, weight: 1, fill: $5})"
    circle_ :: Double -> Double -> Double -> JSString -> Bool -> IO JSVal

foreign import javascript unsafe
    "L['polyline']($1, {color: $2, opacity: 0.6, dashArray: '20,15', lineJoin: 'round'})"
    polyline_ :: JSVal -> JSString -> IO JSVal

foreign import javascript unsafe
    "$1['getBounds']()"
    getBounds_ :: JSVal -> IO JSVal

foreign import javascript unsafe
    "L.latLng($1, $2)"
    latLng_ :: Double -> Double -> IO JSVal

foreign import javascript unsafe
    "$1['toBounds']($2)"
    latLngRadiusBounds_ :: JSVal -> Double -> IO JSVal

foreign import javascript unsafe
    "$1['extend']($2)"
    extendBounds_ :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe
    "$1['fitBounds']($2)"
    fitBounds_ :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe
    "$1['setView']($2.getCenter())"
    panToBounds_ :: JSVal -> JSVal -> IO ()

map :: IsElement e => e -> IO Map
map e =
    Map <$> (map_ . unElement . toElement $ e)

mapSetView :: Map -> (Double, Double) -> Int -> IO ()
mapSetView lm (lat, lng) zoom =
    mapSetView_ (unMap lm) lat lng zoom

mapInvalidateSize :: Map -> IO ()
mapInvalidateSize lmap =
    mapInvalidateSize_ (unMap lmap)

layerGroup :: Polyline -> [Marker] -> IO LayerGroup
layerGroup line xs = do
    ys <- toJSVal $ unMarker <$> xs
    fg <- layerGroup_ (unPolyline line) ys
    return $ LayerGroup fg

layerGroupAddToMap :: LayerGroup -> Map -> IO ()
layerGroupAddToMap x lmap = addToMap_ (unLayerGroup x) (unMap lmap)

layersControl
    :: TileLayer
    -> Map
    -> LayerGroup -- ^ Point to point course line
    -> LayerGroup -- ^ Optimal route of the course
    -> LayerGroup -- ^ Subset of the optimal route's course line
    -> LayerGroup -- ^ Optimal route through the waypoints of the speed section
    -> IO ()
layersControl x lmap course taskRoute taskRouteSubset speedRoute =
    layersControl_
        (unTileLayer x)
        (unMap lmap)
        (unLayerGroup course)
        (unLayerGroup taskRoute)
        (unLayerGroup taskRouteSubset)
        (unLayerGroup speedRoute)

tileLayer :: String -> Int -> IO TileLayer
tileLayer src maxZoom =
    TileLayer <$> tileLayer_ (toJSString src) maxZoom

-- | Adds the tile layer to the map. The layers control does this too.
tileLayerAddToMap :: TileLayer -> Map -> IO ()
tileLayerAddToMap x lmap = addToMap_ (unTileLayer x) (unMap lmap)

marker :: (Double, Double) -> IO Marker
marker (lat, lng) =
    Marker <$> marker_ lat lng

markerAddToMap :: Marker -> Map -> IO ()
markerAddToMap x lmap = addToMap_ (unMarker x) (unMap lmap)

markerPopup :: Marker -> String -> IO ()
markerPopup x msg =
    markerPopup_ (unMarker x) (toJSString msg)

circle :: (Double, Double) -> Double -> String -> Bool -> IO Circle
circle (lat, lng) radius color fill =
    Circle <$> circle_ lat lng radius (toJSString color) fill

circleAddToMap :: Circle -> Map -> IO ()
circleAddToMap x lmap = addToMap_ (unCircle x) (unMap lmap)

polyline :: [(Double, Double)] -> String -> IO Polyline
polyline xs color = do
    ys <- toJSVal $ (\ (lat, lng) -> [ lat, lng ]) <$> xs
    Polyline <$> polyline_ ys (toJSString color)

polylineAddToMap :: Polyline -> Map -> IO ()
polylineAddToMap x lmap = addToMap_ (unPolyline x) (unMap lmap)

circleBounds :: Circle -> IO LatLngBounds
circleBounds x =
    LatLngBounds <$> getBounds_ (unCircle x)

polylineBounds :: Polyline -> IO LatLngBounds
polylineBounds x =
    LatLngBounds <$> getBounds_ (unPolyline x)

extendBounds :: LatLngBounds -> LatLngBounds -> IO LatLngBounds
extendBounds x y =
    LatLngBounds <$> extendBounds_ (unLatLngBounds x) (unLatLngBounds y)

fitBounds :: Map -> LatLngBounds -> IO ()
fitBounds lm bounds = fitBounds_ (unMap lm) (unLatLngBounds bounds)

panToBounds :: Map -> LatLngBounds -> IO ()
panToBounds lm bounds = panToBounds_ (unMap lm) (unLatLngBounds bounds)

latLngBounds :: [(Double, Double, Double)] -> IO LatLngBounds
latLngBounds [] = fail "Empty list passed to latLngBounds"
latLngBounds xs = do
    (y : ys) :: [JSVal] <- sequence $ f <$> xs
    bounds :: JSVal <- foldr g (pure y) ys
    return $ LatLngBounds bounds
    where
        f (lat, lng, radius) = do
            ll <- latLng_ lat lng
            latLngRadiusBounds_ ll (2 * radius)

        g x y = do
            y' <- y
            extendBounds_ x y'
