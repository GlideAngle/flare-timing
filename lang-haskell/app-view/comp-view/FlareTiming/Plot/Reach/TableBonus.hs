{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module FlareTiming.Plot.Reach.TableBonus (tablePilotReachBonus) where

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

tablePilotReachBonus
    :: MonadWidget t m
    => Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotReachBonus sEx xs xsBonus select = do
    let tdFoot = elAttr "td" ("colspan" =: "10")
    let foot = el "tr" . tdFoot . text
    let w = ffor xs (pilotIdsWidth . fmap fst)

    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "5")
                        $ text "Reach (km)"
                    elAttr "th" (("colspan" =: "4") <> ("class" =: "th-reach-frac"))
                        $ text "Fraction"

                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Flown"
                    elClass "th" "th-plot-reach-bonus" $ text "Scored †"
                    elClass "th" "th-plot-reach-bonus-diff" $ text "Δ"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    elClass "th" "th-plot-frac" $ text "Flown"
                    elClass "th" "th-plot-frac-bonus" $ text "Scored ‡"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    el "th" $ text ""

                    return ()

            ev <- dyn $ ffor2 xsBonus sEx (\br sEx' -> do
                    let mapR = Map.fromList br
                    let mapN = Map.fromList sEx'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowReachBonus w select mapR mapN) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

            el "tfoot" $ do
                foot "† Reach as scored."
                foot "Δ Altitude bonus reach."
                foot "‡ The fraction of reach points as scored."

            return ev
    switchHold never ev

rowReachBonus
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map Pilot TrackReach
    -> Map Pilot Alt.AltBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m (Event t Pilot)
rowReachBonus w select mapR mapN p tr = do
    pilot <- sample $ current p

    (eReach, eReachDiff, eFrac
        , yReach, yReachDiff
        , yFrac, yFracDiff) <- sample . current
            $ ffor2 p tr (\p' TrackReach{reach = reachF} ->
                fromMaybe ("", "", "", "", "", "", "") $ do
                    TrackReach{reach = reachE, frac = fracE} <- Map.lookup pilot mapR

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
                        ( showPilotDistance 1 $ reachE
                        , showPilotDistanceDiff 1 reachF reachE
                        , showReachFrac fracE

                        , showPilotDistance 1 reachN
                        , showPilotDistanceDiff 1 reachN reachE

                        , quieten $ showReachFrac rFracN
                        , quieten $ showReachFracDiff rFracN fracE
                        ))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance 1 . reach <$> tr
        elClass "td" "td-plot-reach-bonus" $ text eReach
        elClass "td" "td-plot-reach-bonus-diff" $ text eReachDiff
        elClass "td" "td-norm" $ text yReach
        elClass "td" "td-norm" $ text yReachDiff

        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> tr
        elClass "td" "td-plot-frac-bonus" $ text eFrac
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
