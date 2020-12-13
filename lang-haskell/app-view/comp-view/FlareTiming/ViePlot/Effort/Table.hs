{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.Effort.Table (tableVieEffort) where

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
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableVieEffort
    :: MonadWidget t m
    => Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackEffort)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableVieEffort sEx xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
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

            dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowEffort w select mapN) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

    switchHold never ev

rowEffort
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map Pilot Alt.AltBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackEffort
    -> m (Event t Pilot)
rowEffort w select mapN p te = do
    pilot <- sample $ current p

    (yEffort, yEffortDiff, yFrac, yFracDiff) <- sample . current
            $ ffor2 p te (\p' TrackEffort{effort, frac} ->
                fromMaybe ("", "", "", "") $ do
                    Alt.AltBreakdown
                        { landedMade = effortN
                        , fractions =
                            Frac.Fractions
                                { reach = rFracN
                                , effort = eFracN
                                , distance = dFracN
                                }
                        } <- Map.lookup p' mapN

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

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-effort" . dynText $ (showPilotDistance 1) . effort <$> te
        elClass "td" "td-norm" $ text yEffort
        elClass "td" "td-norm" $ text yEffortDiff

        el "td" . dynText $ showEffortFrac . frac <$> te
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
