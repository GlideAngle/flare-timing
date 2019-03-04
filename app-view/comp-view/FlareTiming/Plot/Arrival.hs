module FlareTiming.Plot.Arrival (arrivalPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Arrival (TrackArrival(..))
import WireTypes.Comp (Discipline(..))
import FlareTiming.Plot.Arrival.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

arrivalPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe [(Pilot, TrackArrival)])
    -> m ()
arrivalPlot hgOrPg av = do
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    _ <- dyn $ ffor hgOrPg (\hgOrPg' ->
                        if hgOrPg' /= HangGliding
                           then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Arrive"
                                    el "p" $ text "Arrival place is not scored in paragliding."
                           else
                                return ())

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
                                    hgPlot (fromMaybe [] <$> av))

                    return ()
