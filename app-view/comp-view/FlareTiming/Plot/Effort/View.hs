module FlareTiming.Plot.Effort.View (effortPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Effort.Plot as P (effortPlot)

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction (EffortFraction(..), showEffortFrac, showEffortFracDiff)
import WireTypes.Effort (TrackEffort(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance, showPilotDistanceDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Pilot (showPilotName)

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
                    elClass "th" "th-plot-effort" $ text "Effort (km)"
                    el "th" $ text "Fraction"
                    el "th" $ text "Pilot"

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
rowEffort mapN p e = do
    (yEffort, yEffortDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p e (\pilot TrackEffort{effort, frac} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown
                                { reachMade = effortN
                                , fractions = Frac.Fractions{effort = fracN}
                                } ->
                            ( showPilotDistance 1 effortN
                            , showPilotDistanceDiff 1 effortN effort

                            , showEffortFrac fracN
                            , showEffortFracDiff fracN frac
                            )

                        _ -> ("", "", "", ""))

    el "tr" $ do
        elClass "td" "td-plot-effort" . dynText $ (showPilotDistance 1) . effort <$> e
        elClass "th" "th-norm" $ text yEffort
        elClass "th" "th-norm" $ text yEffortDiff

        el "td" . dynText $ showEffortFrac . frac <$> e
        elClass "th" "th-norm" $ text yFrac
        elClass "th" "th-norm" $ text yFracDiff

        el "td" . dynText $ showPilotName <$> p

        return ()

