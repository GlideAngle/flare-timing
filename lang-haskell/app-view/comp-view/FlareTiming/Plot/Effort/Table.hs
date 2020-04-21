module FlareTiming.Plot.Effort.Table (tableEffort) where

import Reflex.Dom
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction
    ( ReachFraction(..), EffortFraction(..), DistanceFraction(..)
    , showEffortFrac, showEffortFracDiff
    )
import WireTypes.Effort (TrackEffort(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)

tableEffort
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackEffort)]
    -> m ()
tableEffort sEx xs = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" (("colspan" =: "3") <> ("class" =: "th-plot-effort"))
                        $ text "Effort (km)"
                    elAttr "th" (("colspan" =: "3") <> ("class" =: "th-effort-frac"))
                        $ text "Fraction"

                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    el "th" $ text ""

                    return ()

            _ <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList xs (uncurry (rowEffort w mapN). splitDynPure))

            return ()
    return ()

rowEffort
    :: MonadWidget t m
    => Dynamic t Int
    -> Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackEffort
    -> m ()
rowEffort w mapN p te = do
    (yEffort, yEffortDiff, yFrac, yFracDiff) <- sample . current
            $ ffor2 p te (\pilot TrackEffort{effort, frac} ->
                fromMaybe ("", "", "", "") $ do
                    Norm.NormBreakdown
                        { landedMade = effortN
                        , fractions =
                            Frac.Fractions
                                { reach = rFracN
                                , effort = eFracN
                                , distance = dFracN
                                }
                        } <- Map.lookup pilot mapN

                    let quieten s =
                            case (rFracN, eFracN, dFracN) of
                                (ReachFraction 0, EffortFraction 0, DistanceFraction 0) -> s
                                (ReachFraction 0, EffortFraction 0, _) -> ""
                                _ -> s

                    return
                        ( showPilotDistance 1 effortN
                        , showPilotDistanceDiff 1 effortN effort

                        , quieten $ showEffortFrac eFracN
                        , quieten $ showEffortFracDiff eFracN frac
                        ))

    el "tr" $ do
        elClass "td" "td-plot-effort" . dynText $ (showPilotDistance 1) . effort <$> te
        elClass "td" "td-norm" $ text yEffort
        elClass "td" "td-norm" $ text yEffortDiff

        el "td" . dynText $ showEffortFrac . frac <$> te
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        el "td" . dynText $ ffor2 w p showPilot

        return ()

