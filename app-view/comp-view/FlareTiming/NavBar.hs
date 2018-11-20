{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module FlareTiming.NavBar(navbar) where

import Reflex.Dom (MonadWidget, elClass, el, text)

navbar :: MonadWidget t m => m ()
navbar =
    elClass "nav" "nav has-shadow" $ do
        elClass "div" "container" $ do
            elClass "div" "nav-left" $ do
                elClass "a" "nav-item disable" $ do
                    elClass "span" "icon" $ do
                        elClass "i" "fa fa-paper-plane-o" $ return ()
                    el "span" $ do
                        text "Flare Timing"
            elClass "div" "nav-right nav-menu" $ do
                elClass "a" "nav-item is-tab is-active" $ do
                    text "Tasks"
                elClass "a" "nav-item is-tab" $ do
                    text "Scores"
                elClass "a" "nav-item is-tab" $ do
                    text "Breakdown"
