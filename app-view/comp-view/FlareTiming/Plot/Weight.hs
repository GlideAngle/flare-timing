module FlareTiming.Plot.Weight (weightPlot) where

import Reflex.Dom

import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight.View as W (hgPlot, pgPlot)
import FlareTiming.Plot.Weight.Working as W (viewWeightWorking)

weightPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
weightPlot hgOrPg tweak alloc = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-4" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $
                    elClass "article" "tile is-child" $ do
                        _ <- dyn $ ffor hgOrPg (\case
                                HangGliding -> do
                                    W.hgPlot tweak alloc
                                    viewWeightWorking HangGliding tweak alloc

                                Paragliding -> do
                                    W.pgPlot tweak alloc
                                    viewWeightWorking Paragliding tweak alloc)

                        return ()
        return ()
