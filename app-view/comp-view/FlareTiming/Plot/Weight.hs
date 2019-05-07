module FlareTiming.Plot.Weight (weightPlot) where

import Reflex.Dom

import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight.View as W (hgPlot, pgPlot)
import FlareTiming.Plot.Weight.Working as W (viewWeightWorking)

weightPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
weightPlot hgOrPg vy vw tweak alloc = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-4 is-parent" $
                elClass "article" "tile is-child" $ do
                    _ <- dyn $ ffor hgOrPg (\case
                            HangGliding -> W.hgPlot tweak alloc
                            Paragliding -> W.pgPlot tweak alloc)

                    return ()

        elClass "div" "tile is-8 is-child" $ do
            _ <- dyn $ ffor hgOrPg (\x -> viewWeightWorking x vy vw tweak alloc)
            return ()

    return ()
