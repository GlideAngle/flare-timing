module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom

import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight (hgPlot, pgPlot)

viewPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
viewPlot hgOrPg tweak alloc = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-4" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $
                    elClass "article" "tile is-child box" $ do
                        _ <- dyn $ ffor hgOrPg (\case
                                HangGliding -> hgPlot tweak alloc
                                Paragliding -> pgPlot tweak alloc)
                        return ()
