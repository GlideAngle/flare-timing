module FlareTiming.Plot.Reach.TableReach (tablePilotReach) where

import Reflex.Dom
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction
    ( ReachFraction(..), EffortFraction(..), DistanceFraction(..)
    , showReachFrac, showReachFracDiff
    )
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (ReachToggle(..), showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReach sEx xs = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Reach (km)"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    elClass "th" "th-plot-frac" $ text "Fraction"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            _ <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList xs (uncurry (rowReach w mapN) . splitDynPure))

            return ()
    return ()

rowReach
    :: MonadWidget t m
    => Dynamic t Int
    -> Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReach w mapN p r = do
    (yReach, yReachDiff, yFrac, yFracDiff) <- sample . current
            $ ffor2 p r (\pilot TrackReach{reach, frac} ->
                fromMaybe ("", "", "", "") $ do
                    Norm.NormBreakdown
                        { reach = ReachToggle{extra = reachN}
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
                        ( showPilotDistance 1 reachN
                        , showPilotDistanceDiff 1 reachN reach

                        , quieten $ showReachFrac rFracN
                        , quieten $ showReachFracDiff rFracN frac
                        ))

    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r
        elClass "td" "td-norm" $ text yReach
        elClass "td" "td-norm" $ text yReachDiff

        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> r
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()
