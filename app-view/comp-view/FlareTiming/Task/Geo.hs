module FlareTiming.Task.Geo (tableGeo) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (pack)

import FlareTiming.Comms
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
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

tableCmp
    :: MonadWidget t m
    => IxTask
    -> m ()
tableCmp ix = do
    let lnTask = getTaskLengthSphericalEdge ix
    ln <- (fmap . fmap) taskLength lnTask
    let d = ffor ln (maybe "" $ \TaskLength{..} ->
                showTaskDistance taskRoute)

    legs <- (fmap . fmap) (T.pack . show . length . taskLegs) lnTask

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Method"
                    elClass "th" "th-geo-distance" $ text "Distance"
                    elClass "th" "th-geo-legs" $ text "Legs"

            _ <- el "tbody" $ do
                el "tr" $ do
                    el "th" $ text "Spherical"
                    elClass "th" "td-geo-distance" $ dynText d
                    elClass "th" "td-geo-legs" $ dynText legs

                el "tr" $ do
                    el "th" $ text "Ellipsoid"
                    elClass "th" "td-geo-distance" $ text "102.9 km"
                    elClass "th" "td-geo-legs" $ text "3"

                el "tr" $ do
                    el "th" $ text "Projection spherical"
                    elClass "th" "td-geo-distance" $ text "102.9 km"
                    elClass "th" "td-geo-legs" $ text "3"

                el "tr" $ do
                    el "th" $ text "Projection ellipsoid"
                    elClass "th" "td-geo-distance" $ text "102.9 km"
                    elClass "th" "td-geo-legs" $ text "3"

                el "tr" $ do
                    el "th" $ text "Projection planar"
                    elClass "th" "td-geo-distance" $ text "102.9 km"
                    elClass "th" "td-geo-legs" $ text "3"

            return ()

    return ()
