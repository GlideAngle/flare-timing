module FlareTiming.Plot.Effort (effortPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Effort (TrackEffort(..))
import WireTypes.Comp (Discipline(..))
import qualified FlareTiming.Plot.Effort.View as V (effortPlot)
import WireTypes.Pilot (Pilot(..))

effortPlot
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe [(Pilot, TrackEffort)])
    -> m ()
effortPlot hgOrPg rh =
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent is-vertical" $ do
                    _ <- dyn $ ffor hgOrPg (\hgOrPg' ->
                        if hgOrPg' /= HangGliding
                           then
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Effort"
                                    el "p" $ text "Effort or distance difficulty is not scored in paragliding."
                           else
                                return ())

                    _ <- dyn $ ffor rh (\case
                            Nothing ->
                                elClass "article" "tile is-child" $ do
                                    elClass "p" "title" $ text "Time"
                                    el "p" $ text "Loading effort ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Effort"
                                    el "p" $ text "No pilots started. There are no distances."

                            _ ->
                                elClass "article" "tile is-child" $
                                    V.effortPlot (fromMaybe [] <$> rh))

                    return ()
