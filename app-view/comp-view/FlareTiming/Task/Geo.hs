module FlareTiming.Task.Geo (tableGeo) where

import Reflex.Dom

tableGeo
    :: MonadWidget t m
    => m ()
tableGeo = do
    elClass "div" "tile is-parent is-6" $ do
        elClass "article" "tile is-child box" $ do
            elClass "p" "title" $ text "Geo distance comparison"
            elClass "div" "content" $
                tableCmp

tableCmp
    :: MonadWidget t m
    => m ()
tableCmp = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Method"
                    elClass "th" "th-geo-distance" $ text "Distance"
                    elClass "th" "th-geo-legs" $ text "Legs"

            _ <- el "tbody" $ do
                el "tr" $ do
                    el "th" $ text "Spherical"
                    elClass "th" "td-geo-distance" $ text "102.9 km"
                    elClass "th" "td-geo-legs" $ text "3"

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
