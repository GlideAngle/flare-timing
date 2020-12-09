module FlareTiming.Task.Geo (tableGeo) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import FlareTiming.Comms
import WireTypes.Route
    ( OptimalRoute(..), TaskLength(..)
    , TrackLine(..), PlanarTrackLine(..), TaskLegs(..)
    , taskLength, taskLegs, showTaskDistance
    )
import FlareTiming.Events (IxTask(..))
import WireTypes.Comp (Comp(..), EarthMath(..), showEarthMath)

tableGeo
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Comp
    -> m ()
tableGeo ix c = do
    elClass "div" "tile is-parent" $ do
        elClass "article" "tile is-child box" $ do
            elClass "p" "title" $ text "Geo distance comparison"
            elClass "div" "content" $
                tableCmp ix c

rowOptimal
    :: MonadWidget t m
    => T.Text
    -> EarthMath
    -> m (Dynamic t (OptimalRoute (Maybe TrackLine)))
    -> m ()
rowOptimal earth eMath lnTask = do
    ln <- (fmap . fmap) taskLength lnTask
    let d = ffor ln (maybe "" $ \TaskLength{..} ->
                showTaskDistance taskRoute)

    legs <- (fmap . fmap) ((maybe "" $ T.pack . show . length . (\TaskLegs{legs} -> legs)) . taskLegs) lnTask
    let em = showEarthMath eMath

    el "tr" $ do
        el "td" $ text earth
        el "td" $ text em
        el "td" $ text earth
        el "td" $ text em
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

rowSpherical :: MonadWidget t m => IxTask -> m ()
rowSpherical ix = do
    pb <- getPostBuild
    let x = holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb
    rowOptimal "Sphere" Haversines x

rowSphericalAlt :: MonadWidget t m => IxTask -> m ()
rowSphericalAlt ix = do
    pb <- getPostBuild
    ln <- holdDyn Nothing =<< getTaskLengthAltSphere ix pb
    rowAlt "Sphere" Haversines ln

rowEllipsoid :: MonadWidget t m => IxTask -> EarthMath -> m ()
rowEllipsoid ix eMath = do
    pb <- getPostBuild
    let x = holdDyn emptyRoute =<< getTaskLengthEllipsoidEdge ix pb
    rowOptimal "Ellipsoid" eMath x

rowEllipsoidAlt :: MonadWidget t m => IxTask -> EarthMath -> m ()
rowEllipsoidAlt ix eMath = do
    pb <- getPostBuild
    ln <- holdDyn Nothing =<< getTaskLengthAltEllipse ix pb
    rowAlt "Ellipsoid" eMath ln

rowTrackLine
    :: MonadWidget t m
    => T.Text
    -> EarthMath
    -> Dynamic t (Maybe TrackLine)
    -> m ()
rowTrackLine earthOut eMath ln = do
    let d = ffor ln (maybe "" $ \TrackLine{distance = x} -> showTaskDistance x)

    let legs =
            ffor ln (maybe "" $ T.pack . show . length . (\TrackLine{legs = xs} -> xs))

    el "tr" $ do
        el "td" $ text earthOut
        el "td" . text $ showEarthMath eMath
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

rowAlt
    :: MonadWidget t m
    => T.Text
    -> EarthMath
    -> Dynamic t (Maybe TrackLine)
    -> m ()
rowAlt earthOut eMath ln = do
    let d = ffor ln (maybe "" $ \TrackLine{distance = x} -> showTaskDistance x)

    let legs =
            ffor ln (maybe "" $ T.pack . show . length . (\TrackLine{legs = xs} -> xs))

    elClass "tr" "norm" $ do
        elAttr "td" ("colspan" =: "2") $ text "✓"
        el "td" $ text earthOut
        el "td" . text $ showEarthMath eMath
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

rowProjectedSphere
    :: MonadWidget t m
    => IxTask
    -> m ()
rowProjectedSphere ix = do
    pb <- getPostBuild
    ln <- holdDyn Nothing =<< getTaskLengthProjectedEdgeSpherical ix pb
    rowTrackLine "Sphere" Haversines ln

rowProjectedEllipsoid
    :: MonadWidget t m
    => IxTask
    -> EarthMath
    -> m ()
rowProjectedEllipsoid ix eMath = do
    pb <- getPostBuild
    ln <- holdDyn Nothing =<< getTaskLengthProjectedEdgeEllipsoid ix pb
    rowTrackLine "Ellipsoid" eMath ln

rowProjectedPlanar
    :: MonadWidget t m
    => IxTask
    -> m ()
rowProjectedPlanar ix = do
    pb <- getPostBuild
    ln <- holdDyn Nothing =<< getTaskLengthProjectedEdgePlanar ix pb
    let d = ffor ln (maybe "" $ \PlanarTrackLine{distance = x} -> showTaskDistance x)

    let legs =
            ffor ln (maybe "" $ T.pack . show . length . (\PlanarTrackLine{legs = xs} -> xs))

    el "tr" $ do
        elAttr "td" ("rowspan" =: "3" <> "class" =: "td-geo-plane" ) $ text "Plane"
        elAttr "td" ("rowspan" =: "3" <> "class" =: "td-geo-pythagorus" ) $ text "Pythagorus"
        el "td" $ text "Plane"
        el "td" $ text "Pythagorus"
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

tableCmp
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Comp
    -> m ()
tableCmp ix c = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-geo-network") $ text "Network Paths Cost *"
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-geo-path") $ text "Shortest Path Cost †"
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-geo-how-far") $ text "Shortest Path Distance"

                el "tr" $ do
                    elClass "th" "th-geo-network-earth" $ text "Earth"
                    elClass "th" "th-geo-network-algo" $ text "Algorithm"
                    elClass "th" "th-geo-path-earth" $ text "Earth"
                    elClass "th" "th-geo-path-algo" $ text "Algorithm"
                    elClass "th" "th-geo-distance" $ text "Distance"
                    elClass "th" "th-geo-legs" $ text "Legs"

            dyn_ $ ffor c (\Comp{earthMath} -> el "tbody" $ do
                    rowProjectedPlanar ix
                    rowProjectedSphere ix
                    rowProjectedEllipsoid ix earthMath

                    rowSpherical ix
                    rowEllipsoid ix earthMath

                    rowSphericalAlt ix
                    rowEllipsoidAlt ix earthMath)

            let tr = el "tr" . elAttr "td" ("colspan" =: "6")
            _ <- el "tfoot" $ do
                tr $ text "* The Earth model and algorithm used when constructing a network of points with distance between pairs as the cost to be minimized when finding the shortest path through the network"
                tr $ text "† The Earth model and algorithm used when reporting the sum of distances between pairs of points along the path as the path distance"
                tr $ text "✓ An expected value as calculated by the official scoring program, FS."

            return ()

    return ()
