module FlareTiming.Plot.Reach.View (reachPlot) where

import Data.Maybe (isJust, fromMaybe)
import Reflex.Dom
import Reflex.Time (delay)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (reachPlot)

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction
    ( ReachFraction(..), EffortFraction(..), DistanceFraction(..)
    , showReachFrac, showReachFracDiff
    )
import WireTypes.Comp (Task(..))
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point
    (ReachToggle(..), PilotDistance(..), showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Pilot (showPilot)

placings :: [TrackReach] -> [[Double]]
placings = fmap xy

xy :: TrackReach -> [Double]
xy TrackReach{reach = PilotDistance x, frac = ReachFraction y} =
    [x, y]

rawReach :: TrackReach -> Double
rawReach TrackReach{reach = PilotDistance x} = x

timeRange :: [TrackReach] -> (Double, Double)
timeRange xs = let rXs = rawReach <$> xs in (minimum rXs, maximum rXs)

reValue :: [(Pilot, TrackReach)] -> [(Pilot, TrackReach)] -> [[Double]]
reValue pxs pys =
    [ [x, y]
    | (_, TrackReach{reach = PilotDistance x}) <- sortOn fst pxs
    | (_, TrackReach{frac = ReachFraction y}) <- sortOn fst pys
    ]

reachPlot
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
reachPlot task sEx reach bonusReach = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-reach") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let xs = snd . unzip $ reach'
                            let tt = timeRange xs
                            _ <-
                                if isJust stopped then
                                    P.reachPlot
                                        (_element_raw elPlot)
                                        tt
                                        (placings xs)
                                        (reValue reach' bonusReach')
                                else
                                    P.reachPlot
                                        (_element_raw elPlot)
                                        tt
                                        (placings xs)
                                        []

                            return ())
                        ]

                    reach' <- sample . current $ reach
                    Task{stopped} <- sample . current $ task
                    bonusReach' <- sample . current $ bonusReach

                return ()

        elClass "div" "tile is-child" $ do
            _ <- dyn $ ffor task (\case
                    Task{stopped = Nothing} -> tablePilotReach sEx reach
                    Task{stopped = Just _} -> tablePilotReachBonus sEx reach bonusReach)

            return ()

    return ()

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReach sEx reach = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Reach (km)"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    elClass "th" "th-plot-frac" $ text "Fraction"
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"

                    el "th" $ text "###-Pilot"

                    return ()

            _ <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList reach (uncurry (rowReach mapN) . splitDynPure))

            return ()
    return ()

rowReach
    :: MonadWidget t m
    => Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReach mapN p r = do
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

        elClass "td" "td-pilot" . dynText $ showPilot <$> p

        return ()

tablePilotReachBonus
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReachBonus sEx reach bonusReach = do
    let tdFoot = elAttr "td" ("colspan" =: "6")
    let foot = el "tr" . tdFoot . text

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "5")
                        $ text "Reach (km)"
                    elAttr "th" (("colspan" =: "4") <> ("class" =: "th-reach-frac"))
                        $ text "Fraction"

                    el "th" $ text "###-Pilot"

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

            _ <- dyn $ ffor2 bonusReach sEx (\br sEx' -> do
                    let mapR = Map.fromList br
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList reach (uncurry (rowReachBonus mapR mapN) . splitDynPure))

            el "tfoot" $ do
                foot "† Reach as scored."
                foot "Δ Altitude bonus reach."
                foot "‡ The fraction of reach points as scored."
            return ()
    return ()

rowReachBonus
    :: MonadWidget t m
    => Map Pilot TrackReach
    -> Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReachBonus mapR mapN p tr = do
    (eReach, eReachDiff, eFrac
        , yReach, yReachDiff
        , yFrac, yFracDiff) <- sample . current
            $ ffor2 p tr (\pilot TrackReach{reach = reachF} ->
                fromMaybe ("", "", "", "", "", "", "") $ do
                    TrackReach{reach = reachE, frac = fracE} <- Map.lookup pilot mapR

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
                        ( showPilotDistance 1 $ reachE
                        , showPilotDistanceDiff 1 reachF reachE
                        , showReachFrac fracE

                        , showPilotDistance 1 reachN
                        , showPilotDistanceDiff 1 reachN reachE

                        , quieten $ showReachFrac rFracN
                        , quieten $ showReachFracDiff rFracN fracE
                        ))

    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance 1 . reach <$> tr
        elClass "td" "td-plot-reach-bonus" $ text eReach
        elClass "td" "td-plot-reach-bonus-diff" $ text eReachDiff
        elClass "td" "td-norm" $ text yReach
        elClass "td" "td-norm" $ text yReachDiff

        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> tr
        elClass "td" "td-plot-frac-bonus" $ text eFrac
        elClass "td" "td-norm" $ text yFrac
        elClass "td" "td-norm" $ text yFracDiff

        elClass "td" "td-pilot" . dynText $ showPilot <$> p

        return ()
