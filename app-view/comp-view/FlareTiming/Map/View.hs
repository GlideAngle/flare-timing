module FlareTiming.Map.View (viewMap) where

import Prelude hiding (map)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
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
    , fitBounds
    , panToBounds
    , latLngBounds
    , layersControl
    )
import WireTypes.Comp (Task(..), SpeedSection, getAllRawZones)
import WireTypes.Zone
    (Zones(..), RawZone(..), RawLatLng(..), RawLat(..), RawLng(..), Radius(..))
import WireTypes.Route (OptimalRoute, TrackLine, taskOptimalRoute)
import qualified FlareTiming.Turnpoint as TP (getName)

data ZoomOrPan = Zoom | Pan deriving Show

zoomButton
    :: MonadWidget t m
    => (RawZone, T.Text)
    -> m (Event t [RawZone])
zoomButton (z, btnClass) = do
    let s = TP.getName z
    (e, _) <- elClass' "a" btnClass $ text s
    return $ [z] <$ domEvent Click e

zoomOrPanIcon :: ZoomOrPan -> T.Text
zoomOrPanIcon Zoom = "fa fa-search-plus"
zoomOrPanIcon Pan = "fa fa-arrows"

taskZoneButtons
    :: MonadWidget t m
    => Task
    -> m ((Dynamic t ZoomOrPan, Dynamic t [RawZone]))
taskZoneButtons t@Task{speedSection} = do
    let zones = getAllRawZones t
    let btn = "button"
    let btnStart = "button has-text-success"
    let btnEnd = "button has-text-danger"

    let zoneClasses =
            maybe
                (zip zones $ repeat btn)
                (\(start, end) ->
                    zipWith
                        (\z i ->
                            let c = if | i == start -> btnStart
                                       | i == end -> btnEnd
                                       | otherwise -> btn
                            in (z, c))
                        zones
                        [1..])

    elClass "div" "buttons has-addons" $ do
        rec (zoom, _) <-
                elClass' "a" "button" $ do
                    elClass "span" "icon is-small" $
                        elDynClass "i" zpClass $ return ()
                    el "span" $ dynText zpText

            zoomOrPan <-
                (fmap . fmap)
                (\case True -> Pan; False -> Zoom)
                (toggle True $ domEvent Click zoom)

            let zpText = ffor zoomOrPan $ T.pack . (++ " to ...") . show
            let zpClass = ffor zoomOrPan zoomOrPanIcon

        (extents, _) <- elAttr' "a" ("class" =: "button") $ text "Extents"
        let allZones = zones <$ domEvent Click extents

        eachZone <- sequence $ zoomButton <$> zoneClasses speedSection
        zs <- holdDyn zones . leftmost $ allZones : eachZone
        return $ (zoomOrPan, zs)

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

viewMap
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (OptimalRoute (Maybe TrackLine))
    -> m ()
viewMap task route = do
    task' <- sample . current $ task
    route' <- sample . current $ route
    let optimal = taskOptimalRoute route'
    map task' optimal

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

    (zoomOrPan, evZoom) <- taskZoneButtons task

    (eCanvas, _) <- elAttr' "div" ("style" =: "height: 680px;width: 100%") $ return ()

    rec performEvent_ $ leftmost
            [ ffor postBuild (\_ -> liftIO $ do
                L.mapInvalidateSize lmap'
                L.fitBounds lmap' bounds'
                return ())

            , updated $ ffor2 zoomOrPan evZoom (\zp zs -> liftIO $ do
                bs <- L.latLngBounds $ zoneToLLR <$> zs

                case zp of
                    Zoom -> L.fitBounds lmap' bs
                    Pan -> L.panToBounds lmap' bs

                return ())
            ]

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

            bounds <- L.latLngBounds $ zoneToLLR <$> xs

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
