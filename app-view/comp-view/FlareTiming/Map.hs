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
    , latLngBounds
    , layersControl
    )
import WireTypes.Comp (Task(..), SpeedSection, getRaceRawZones)
import WireTypes.Zone
    (Zones(..), RawZone(..), RawLatLng(..), RawLat(..), RawLng(..), Radius(..))
import qualified FlareTiming.Turnpoint as TP (getName)

zoomButton
    :: MonadWidget t m
    => RawZone
    -> m (Event t [RawZone])
zoomButton z = do
    let s = TP.getName z
    (e, _) <- elAttr' "a" ("class" =: "button") $ text s
    return $ [z] <$ domEvent Click e

taskTileZones
    :: MonadWidget t m
    => Task
    -> m ([Event t [RawZone]])
taskTileZones t = do
    let zs = getRaceRawZones t
    elClass "div" "buttons has-addons" $ do
        (e, _) <- elAttr' "a" ("class" =: "button") $ text "Zoom to Extents"
        let v = zs <$ domEvent Click e
        vs <- sequence $ zoomButton <$> zs
        return $ v : vs

turnpoint :: String -> RawZone -> IO (L.Marker, L.Circle)
turnpoint
    color
    RawZone
        { lat = RawLat lat'
        , lng = RawLng lng'
        , radius = Radius r
        } = do
    xMark <- L.marker latLng
    xCyl <- L.circle latLng r color
    return (xMark, xCyl)
    where
        latLng = (fromRational lat', fromRational lng')

zoneToLL :: RawZone -> (Double, Double)
zoneToLL RawZone{lat = RawLat lat', lng = RawLng lng'} =
    (fromRational lat', fromRational lng')

zoneToLLR :: RawZone -> (Double, Double, Double)
zoneToLLR RawZone{lat = RawLat lat', lng = RawLng lng', radius = Radius r} =
    (fromRational lat', fromRational lng', r)

rawToLL :: RawLatLng -> (Double, Double)
rawToLL RawLatLng{lat = RawLat lat', lng = RawLng lng'} =
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

map task@Task{zones = Zones{raw = xs}, speedSection} ys = do
    let tpNames = fmap (\RawZone{..} -> zoneName) xs
    postBuild <- delay 1 =<< getPostBuild

    evZoomToExtent : evZooms <- taskTileZones task

    (eCanvas, _) <- elAttr' "div" ("style" =: "height: 680px;width: 100%") $ return ()

    rec performEvent_ $ leftmost
            ([ ffor postBuild (\_ -> liftIO $ do
                L.mapInvalidateSize lmap'
                L.fitBounds lmap' bounds'
                return ())

            , ffor evZoomToExtent (\_ -> liftIO $ do
                L.fitBounds lmap' bounds'
                return ())
            ]
            <>
            (fmap (\zs -> liftIO $ do
                zBounds <- L.latLngBounds $ zoneToLLR <$> zs
                L.fitBounds lmap' zBounds
                return ())
            <$> evZooms))

        (lmap', bounds') <- liftIO $ do
            lmap <- L.map (_element_raw eCanvas)
            L.mapSetView lmap (zoneToLL $ head xs) 11

            mapLayer <-
                -- SEE: http://leaflet-extras.github.io/leaflet-providers/preview/
                L.tileLayer
                    "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
                    17

            let len = length xs
            let cs = zoneColors len speedSection

            xMarks :: [(L.Marker, L.Circle)]
                <- sequence $ zipWith turnpoint cs xs

            _ <- sequence $ fmap
                (\ (tpName, (xMark, xCyl)) -> do
                    L.markerAddToMap xMark lmap
                    L.markerPopup xMark tpName
                    L.circleAddToMap xCyl lmap
                    return ())
                (zip tpNames xMarks)

            let xPts :: [(Double, Double)] = fmap zoneToLL xs
            let yPts :: [(Double, Double)] = fmap rawToLL ys

            courseLine <- L.polyline xPts "gray"
            routeLine <- L.polyline yPts "red"

            -- NOTE: Adding the route now so that it displays by default but
            -- can also be hidden via the layers control. The course line is
            -- not added by default but can be shown via the layers control.
            L.polylineAddToMap routeLine lmap
            L.layersControl mapLayer lmap courseLine routeLine

            bounds <- L.polylineBounds courseLine

            return (lmap, bounds)

    return ()

blues :: [String]
blues = repeat "blue"

yellows :: [String]
yellows = repeat "yellow"

zoneColors :: Int -> SpeedSection -> [String]
zoneColors _ Nothing = blues
zoneColors len (Just (start, end)) =
    if len < 2 then blues else
    prolog <> ["green"] <> xs' <> ["red"] <> yellows
    where
        -- NOTE: The speed section uses 1-based indexing.
        start' = fromIntegral start
        end' = fromIntegral end

        prolog = take (start' - 1) $ yellows
        xs' = take ((end' - start' + 1) - 2) blues
