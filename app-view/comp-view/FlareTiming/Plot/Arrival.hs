module FlareTiming.Plot.Arrival (arrivalPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Arrival (TrackArrival(..))
import WireTypes.Comp (Discipline(..))
import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (Allocation(..))
import FlareTiming.Plot.Weight.View as W (hgPlot, pgPlot)
import FlareTiming.Plot.Arrival.View as A (hgPlot)
import WireTypes.Pilot (Pilot(..))

arrivalPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe PilotsFlying)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t (Maybe [(Pilot, TrackArrival)])
    -> m ()
arrivalPlot hgOrPg pf alloc av = do
    elClass "div" "tile is-ancestor" $ do
        _ <- dyn $ ffor3 hgOrPg pf alloc (\hgOrPg' pf' alloc' ->
            if hgOrPg' /= HangGliding then return () else
                elClass "div" "tile is-8" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent" $ do
                            _ <- dyn $ ffor av (\case
                                    Nothing ->
                                        elClass "article" "tile is-child box" $ do
                                            elClass "p" "title" $ text "Arrivals"
                                            el "p" $ text "Loading arrivals ..."

                                    Just [] ->
                                        elClass "article" "tile is-child notification is-warning" $ do
                                            elClass "p" "title" $ text "Arrivals"
                                            el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals"

                                    _ ->
                                        elClass "article" "tile is-child box" $
                                            A.hgPlot pf' alloc' (fromMaybe [] <$> av))

                            return ())

        return ()
