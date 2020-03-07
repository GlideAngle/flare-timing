module FlareTiming.Plot.LeadCoef (leadCoefPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom

import WireTypes.Comp (Tweak(..), LwScaling(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Lead (TrackLead(..))
import qualified FlareTiming.Plot.LeadCoef.View as V (leadCoefPlot)
import WireTypes.Pilot (Pilot(..))

leadCoefPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t (Maybe [(Pilot, TrackLead)])
    -> m ()
leadCoefPlot tweak sEx ld =
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

                                    V.leadCoefPlot tweak sEx (fromMaybe [] <$> ld))

                    return ()
