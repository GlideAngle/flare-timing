module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom

tableTurnpoints
    :: MonadWidget t m
    => m ()
tableTurnpoints =
    elClass "table" "table" $
        el "thead" $ do
            el "tr" $ do
                el "th" $ text "No"
                el "th" $ text "Id"
                el "th" $ text "Radius"
                el "th" $ text "Latitude"
                el "th" $ text "Longitude"
