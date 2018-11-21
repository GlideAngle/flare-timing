module FlareTiming.NavBar(navbar) where

import Reflex.Dom (MonadWidget, elClass, el, text)

navbar :: MonadWidget t m => m ()
navbar =
    elClass "nav" "navbar" $ do
        elClass "div" "navbar-brand" $ do
            elClass "a" "navbar-item" $ do
                elClass "span" "icon" $ do
                    elClass "i" "fa fa-paper-plane-o" $ return ()
                el "span" $ text "Flare Timing"
        elClass "div" "navbar-menu is-active" $ do
            elClass "a" "navbar-item " $ text "Tasks"
            elClass "a" "navbar-item" $ text "Scores"
            elClass "a" "navbar-item" $ text "Breakdown"
