module FlareTiming.Plot.Effort.View (effortPlot) where

import Data.Maybe (fromMaybe)
import Reflex.Dom
import Reflex.Time (delay)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Effort.Plot as P (effortPlot)

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction
    ( ReachFraction(..), EffortFraction(..), DistanceFraction(..)
    , showEffortFrac, showEffortFracDiff
    )
import WireTypes.Effort (TrackEffort(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Pilot (showPilot)

placings :: [TrackEffort] -> [[Double]]
placings = fmap xy

xy :: TrackEffort -> [Double]
xy TrackEffort{effort = PilotDistance x, frac = EffortFraction y} =
    [x, y]

timeRange :: [TrackEffort] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackEffort{effort = PilotDistance x} -> x) <$> xs

effortPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackEffort)]
    -> m ()
effortPlot sEx tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent is-vertical" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-effort") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.effortPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                                return ())
                            ]

                        elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $
                                    el "li" $ do
                                        elClass "span" "legend-effort" $ text "- - -"
                                        text " line of constant effort"

                        tm' <- sample . current $ tm

                    return ()

        elClass "div" "tile is-child" $ tablePilot sEx tm

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackEffort)]
    -> m ()
tablePilot sEx xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" (("colspan" =: "3") <> ("class" =: "th-plot-effort"))
                        $ text "Effort (km)"
                    elAttr "th" (("colspan" =: "3") <> ("class" =: "th-effort-frac"))
                        $ text "Fraction"

                    el "th" $ text "###-Pilot"

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
                        simpleList xs (uncurry (rowEffort mapN). splitDynPure))

            return ()
    return ()

rowEffort
    :: MonadWidget t m
    => Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackEffort
    -> m ()
rowEffort mapN p te = do
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

        el "td" . dynText $ showPilot <$> p

        return ()

