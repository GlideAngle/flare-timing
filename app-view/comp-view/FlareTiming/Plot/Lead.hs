module FlareTiming.Plot.Lead (leadPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Comp (Tweak(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Lead (TrackLead(..))
import FlareTiming.Plot.Lead.View (hgPlot)
import WireTypes.Pilot (Pilot(..))

leadPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t (Maybe [(Pilot, TrackLead)])
    -> m ()
leadPlot tweak sEx ld =
    elClass "div" "tile is-ancestor" $
        elClass "div" "tile is-12" $
            elClass "div" "tile" $
                elClass "div" "tile is-parent" $ do
                    _ <- dyn $ ffor ld (\case
                            Nothing ->
                                elClass "article" "tile is-child" $ do
                                    elClass "p" "title" $ text "Leading"
                                    el "p" $ text "Loading arrivals ..."

                            Just [] ->
                                elClass "article" "tile is-child notification is-warning" $ do
                                    elClass "p" "title" $ text "Leading"
                                    el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals"

                            _ ->
                                elClass "article" "tile is-child" $
                                    hgPlot tweak sEx (fromMaybe [] <$> ld))

                    return ()
