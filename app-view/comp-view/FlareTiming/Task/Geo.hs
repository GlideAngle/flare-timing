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

tableGeo
    :: MonadWidget t m
    => IxTask
    -> m ()
tableGeo ix = do
    elClass "div" "tile is-parent is-6" $ do
        elClass "article" "tile is-child box" $ do
            elClass "p" "title" $ text "Geo distance comparison"
            elClass "div" "content" $
                tableCmp ix

rowOptimal
    :: MonadWidget t m
    => T.Text
    -> m (Dynamic t (OptimalRoute (Maybe TrackLine)))
    -> m ()
rowOptimal rowHeader lnTask = do
    ln <- (fmap . fmap) taskLength lnTask
    let d = ffor ln (maybe "" $ \TaskLength{..} ->
                showTaskDistance taskRoute)

    legs <- (fmap . fmap) ((maybe "" $ T.pack . show . length . (\TaskLegs{legs} -> legs)) . taskLegs) lnTask

    el "tr" $ do
        el "th" $ text rowHeader
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

rowSpherical :: MonadWidget t m => IxTask -> m ()
rowSpherical = rowOptimal "Spherical" . getTaskLengthSphericalEdge

rowEllipsoid :: MonadWidget t m => IxTask -> m ()
rowEllipsoid = rowOptimal "Ellipsoid" . getTaskLengthEllipsoidEdge

rowTrackLine
    :: MonadWidget t m
    => T.Text
    -> Dynamic t (Maybe TrackLine)
    -> m ()
rowTrackLine rowHeader ln = do
    let d = ffor ln (maybe "" $ \TrackLine{distance = x} -> showTaskDistance x)

    let legs =
            ffor ln (maybe "" $ T.pack . show . length . (\TrackLine{legs = xs} -> xs))

    el "tr" $ do
        el "th" $ text rowHeader
        elClass "td" "td-geo-distance" $ dynText d
        elClass "td" "td-geo-legs" $ dynText legs

rowProjectedSphere
    :: MonadWidget t m
    => IxTask
    -> m ()
rowProjectedSphere ix = do
    ln  <- getTaskLengthProjectedEdgeSpherical ix
    rowTrackLine "Projected sphere" ln

rowProjectedEllipsoid
    :: MonadWidget t m
    => IxTask
    -> m ()
rowProjectedEllipsoid ix = do
    ln <- getTaskLengthProjectedEdgeEllipsoid ix
    rowTrackLine "Projected ellipsoid" ln

rowProjectedPlanar
    :: MonadWidget t m
    => IxTask
    -> m ()
rowProjectedPlanar ix = do
    ln <- getTaskLengthProjectedEdgePlanar ix
    let d = ffor ln (maybe "" $ \PlanarTrackLine{distance = x} -> showTaskDistance x)

    let legs =
            ffor ln (maybe "" $ T.pack . show . length . (\PlanarTrackLine{legs = xs} -> xs))

    el "tr" $ do
        el "th" $ text "Projected planar"
        elClass "th" "td-geo-distance" $ dynText d
        elClass "th" "td-geo-legs" $ dynText legs

tableCmp
    :: MonadWidget t m
    => IxTask
    -> m ()
tableCmp ix = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Method"
                    elClass "th" "th-geo-distance" $ text "Distance"
                    elClass "th" "th-geo-legs" $ text "Legs"

            _ <- el "tbody" $ do
                rowSpherical ix
                rowEllipsoid ix

                rowProjectedSphere ix
                rowProjectedEllipsoid ix
                rowProjectedPlanar ix

            return ()

    return ()
