module FlareTiming.Map (map) where

import Prelude hiding (map)
import Reflex.Dom
import Reflex.Time (delay)
import Control.Monad (sequence)
import Control.Monad.IO.Class (liftIO)

import qualified FlareTiming.Map.Leaflet as L
    ( Marker(..)
    , Circle(..)
    , map
    , mapSetView
    , tileLayer
    , tileLayerAddToMap
    , marker
    , markerAddToMap
    , markerPopup
    , mapInvalidateSize
    , circle
    , circleAddToMap
    , polyline 
    , polylineAddToMap
    , polylineBounds
    , fitBounds
    )
import WireTypes.Comp (Task(..))
import WireTypes.Zone
    (Zones(..), RawZone(..), RawLatLng(..), RawLat(..), RawLng(..), Radius(..))

turnpoint :: RawZone -> IO (L.Marker, L.Circle)
turnpoint
    RawZone
        { lat = RawLat lat'
        , lng = RawLng lng'
        , radius = Radius r
        } = do
    xMark <- L.marker latLng
    xCyl <- L.circle latLng r
    return (xMark, xCyl)
    where
        latLng = (fromRational lat', fromRational lng')

zoneToLatLng :: RawZone -> (Double, Double)
zoneToLatLng RawZone{lat = RawLat lat', lng = RawLng lng'} =
    (fromRational lat', fromRational lng')

rawToLatLng :: RawLatLng -> (Double, Double)
rawToLatLng RawLatLng{lat = RawLat lat', lng = RawLng lng'} =
    (fromRational lat', fromRational lng')

map
    :: MonadWidget t m
    => Task
    -> [RawLatLng]
    -> m ()

map Task{zones = Zones{raw = []}} _ = do
    el "p" $ text "The task has no turnpoints."
    return ()

map _ [] = do
    el "p" $ text "The task optimal route has no turnpoints."
    return ()

map Task{zones = Zones{raw = xs}} ys = do
    let tpNames = fmap (\RawZone{..} -> zoneName) xs
    postBuild <- delay 1 =<< getPostBuild
    (e, _) <- elAttr' "div" ("style" =: "height: 680px;width: 100%") $ return ()
    rec performEvent_ $ fmap
                            (\_ -> liftIO $ do
                                L.mapInvalidateSize lmap'
                                L.fitBounds lmap' bounds'
                                return ()
                            )
                            postBuild
        (lmap', bounds') <- liftIO $ do
            lmap <- L.map (_element_raw e)
            L.mapSetView lmap (zoneToLatLng $ head xs) 11

            layer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                L.tileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17

            L.tileLayerAddToMap layer lmap

            xMarks :: [(L.Marker, L.Circle)] <- sequence $ fmap turnpoint xs

            _ <- sequence $ fmap
                (\ (tpName, (xMark, xCyl)) -> do
                    L.markerAddToMap xMark lmap
                    L.markerPopup xMark tpName
                    L.circleAddToMap xCyl lmap
                    return ())
                (zip tpNames xMarks)

            let xPts :: [(Double, Double)] = fmap zoneToLatLng xs
            let yPts :: [(Double, Double)] = fmap rawToLatLng ys

            courseLine <- L.polyline xPts "gray"
            L.polylineAddToMap courseLine lmap

            routeLine <- L.polyline yPts "red"
            L.polylineAddToMap routeLine lmap

            bounds <- L.polylineBounds courseLine

            return (lmap, bounds)

    return ()
