module FlareTiming.Plot.View (viewPlot) where

import Reflex.Dom

import WireTypes.Arrival (TrackArrival(..))
import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight.View as W (hgPlot, pgPlot)
import FlareTiming.Plot.Arrival.View as A (hgPlot)
import WireTypes.Pilot (Pilot(..))

viewPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe PilotsFlying)
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
viewPlot hgOrPg pf tweak alloc av = do
    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile is-4" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $
                    elClass "article" "tile is-child box" $ do
                        _ <- dyn $ ffor hgOrPg (\case
                                HangGliding -> W.hgPlot tweak alloc
                                Paragliding -> W.pgPlot tweak alloc)

                        return ()

        _ <- dyn $ ffor3 hgOrPg pf alloc (\hgOrPg' pf' alloc' ->
                if hgOrPg' /= HangGliding then return () else do
                    elClass "div" "tile is-8" $
                        elClass "div" "tile" $
                            elClass "div" "tile is-parent" $
                                elClass "article" "tile is-child box" $
                                    A.hgPlot pf' alloc' av)

        return ()
