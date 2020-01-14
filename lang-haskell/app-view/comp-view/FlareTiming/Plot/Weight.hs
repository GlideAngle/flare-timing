module FlareTiming.Plot.Weight (weightPlot) where

import Reflex.Dom

import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import WireTypes.Route (TaskLength(..))
import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight.View as W (hgWeightPlot, pgWeightPlot)
import FlareTiming.Plot.Weight.Working as W (viewWeightWorking)

weightPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t (Maybe TaskLength)
    -> m ()
weightPlot hgOrPg' tweak vy vw alloc ln = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-6" $
            elClass "div" "tile is-parent" $
                elClass "article" "tile is-child" $ do
                    _ <- dyn $ ffor hgOrPg' (\case
                            HangGliding -> W.hgWeightPlot tweak alloc
                            Paragliding -> W.pgWeightPlot tweak alloc)

                    return ()

        elClass "div" "tile is-child" $ do
            _ <- dyn $ ffor hgOrPg' (\hgOrPg -> viewWeightWorking hgOrPg tweak vy vw alloc ln)
            return ()

    return ()
