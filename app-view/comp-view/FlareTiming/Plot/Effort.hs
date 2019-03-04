module FlareTiming.Plot.Effort (effortPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Effort (TrackEffort(..))
import FlareTiming.Plot.Effort.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

effortPlot
    :: MonadWidget t m
    => Dynamic t (Maybe [(Pilot, TrackEffort)])
    -> m ()
effortPlot rh =
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $ do
                    _ <- dyn $ ffor rh (\case
                            Nothing ->
                                elClass "article" "tile is-child box" $ do
                                    elClass "p" "title" $ text "Time"
                                    el "p" $ text "Loading effort ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Effort"
                                    el "p" $ text "No pilots started. There are no distances."

                            _ ->
                                elClass "article" "tile is-child box" $
                                    hgPlot (fromMaybe [] <$> rh))

                    return ()
