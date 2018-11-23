module FlareTiming.Map (map) where

import Prelude hiding (map)
import Reflex.Dom
    ( MonadWidget
    , (=:)
    , elAttr'
    , getPostBuild
    , performEvent_
    , _element_raw
    , el
    , text
    )
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
import Data.Flight.Types
    ( Task(..)
    , Zones(..)
    , RawZone(..)
    , RawLat(..)
    , RawLng(..)
    , Radius(..)
    )

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

toLatLng :: RawZone -> (Double, Double)
toLatLng RawZone{lat = RawLat lat', lng = RawLng lng'} =
    (fromRational lat', fromRational lng')

map :: MonadWidget t m => Task -> m ()

map Task{zones = Zones{raw = []}} = do
    el "p" $ text "The task has no turnpoints."
    return ()

map Task{zones = Zones{raw = xs}}= do
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
            L.mapSetView lmap (toLatLng $ head xs) 11

            layer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                L.tileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17

            L.tileLayerAddToMap layer lmap

            zs :: [(L.Marker, L.Circle)] <- sequence $ fmap turnpoint xs

            _ <- sequence $ fmap
                (\ (tpName, (xMark, xCyl)) -> do
                    L.markerAddToMap xMark lmap
                    L.markerPopup xMark tpName
                    L.circleAddToMap xCyl lmap
                    return ())
                (zip tpNames zs)

            let pts :: [(Double, Double)] = fmap toLatLng xs

            courseLine <- L.polyline pts
            L.polylineAddToMap courseLine lmap

            bounds <- L.polylineBounds courseLine

            return (lmap, bounds)

    return ()
