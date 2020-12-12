module FlareTiming.ViePlot.LeadCoef (leadCoefViePlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Comp (Tweak(..), LwScaling(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Lead (TrackLead(..))
import qualified FlareTiming.ViePlot.LeadCoef.View as V (leadCoefViePlot)
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Events (IxTask(..))

leadCoefViePlot
    :: MonadWidget t m
    => IxTask
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t (Maybe [(Pilot, TrackLead)])
    -> m ()
leadCoefViePlot ix tweak sEx ld =
    elClass "div" "tile is-ancestor" $ do
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
                                    el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals."

                            _ -> do
                                let notice =
                                        elClass "article" "notification is-warning" $
                                            el "p" $ text "No points awarded for leading."

                                elClass "article" "tile is-child" $ do
                                    _ <- dyn $ ffor tweak (\case
                                        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> notice
                                        _ -> return ())

                                    V.leadCoefViePlot ix tweak sEx (fromMaybe [] <$> ld))

                    return ()
