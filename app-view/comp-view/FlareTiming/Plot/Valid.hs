module FlareTiming.Plot.Valid (validPlot) where

import Reflex.Dom

import FlareTiming.Plot.Valid.View (hgPlot)

validPlot
    :: MonadWidget t m
    => m ()
validPlot = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-4 is-parent" $
                elClass "article" "tile is-child" $
                    hgPlot
