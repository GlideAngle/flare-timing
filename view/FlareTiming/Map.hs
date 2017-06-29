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
    , circle
    , circleAddToMap
    , polyline 
    , polylineAddToMap
    , circleBounds
    , polylineBounds
    , extendBounds
    , fitBounds
    )

map :: MonadWidget t m => m ()
map = do
    postBuild <- delay 0 =<< getPostBuild
    (e, _) <- elAttr' "div" ("style" =: "height: 240px;width: 320px") $ return ()
    rec performEvent_ $ fmap
                            (\_ -> liftIO $ do
                                L.mapInvalidateSize lmap'
                                L.fitBounds lmap' bounds'
                                return ()
                            )
                            postBuild
        (lmap', bounds') <- liftIO $ do
            lmap <- L.map (_element_raw e)
            L.mapSetView lmap (negate 33.36137, 147.93207) 11

            layer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                L.tileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17

            L.tileLayerAddToMap layer lmap

            startMark <- L.marker pt1
            L.markerAddToMap startMark lmap
            L.markerPopup startMark "FORBES"

            launch <- L.circle pt1 100
            L.circleAddToMap launch lmap

            start <- L.circle pt1 10000
            L.circleAddToMap start lmap

            tp1Mark <- L.marker pt2
            L.markerAddToMap tp1Mark lmap
            L.markerPopup tp1Mark "PINEY"

            tp1 <- L.circle pt2 400
            L.circleAddToMap tp1 lmap

            tp2Mark <- L.marker pt3
            L.markerAddToMap tp2Mark lmap
            L.markerPopup tp2Mark "EUGOWR"

            tp2 <- L.circle pt3 400
            L.circleAddToMap tp2 lmap

            goalMark <- L.marker pt4
            L.markerAddToMap goalMark lmap
            L.markerPopup goalMark "GOALD1"

            goal <- L.circle pt4 400
            L.circleAddToMap goal lmap

            courseLine <- L.polyline [ pt1, pt2, pt3, pt4 ]
            L.polylineAddToMap courseLine lmap

            startBounds <- L.circleBounds start
            courseBounds <- L.polylineBounds courseLine
            bounds <- L.extendBounds courseBounds startBounds

            return (lmap, bounds)

    return ()
    where
        pt1 = (negate 33.36137, 147.93207)
        pt2 = (negate 33.85373, 147.94195)
        pt3 = (negate 33.4397, 148.34533)
        pt4 = (negate 33.61965, 148.4099)
