{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.Reach.TableReach (tableViePilotReach) where

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
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableViePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableViePilotReach sEx xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
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

            dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowReach w select mapN) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

    switchHold never ev

rowReach
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map Pilot Alt.AltBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m (Event t Pilot)
rowReach w select mapN p r = do
    pilot <- sample $ current p

    (yReach, yReachDiff, yFrac, yFracDiff) <- sample . current
            $ ffor2 p r (\p' TrackReach{reach, frac} ->
                fromMaybe ("", "", "", "") $ do
                    Alt.AltBreakdown
                        { reach = ReachToggle{extra = reachN}
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
                        ( showPilotDistance 1 reachN
                        , showPilotDistanceDiff 1 reachN reach

                        , quieten $ showReachFrac rFracN
                        , quieten $ showReachFracDiff rFracN frac
                        ))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r
        elClass "td" "td-norm" $ text yReach
        elClass "td" "td-norm" $ text yReachDiff

        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> r
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    let ePilot = const pilot <$> domEvent Click eRow
    return ePilot
