module FlareTiming.Plot.Time.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Time.Plot as P (hgPlot)

import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..), SpeedFraction(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (StartGate)
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (showHmsForHours, showHours)
import FlareTiming.Task.Score.Show

placings :: [TrackSpeed] -> [[Double]]
placings = fmap xy

xy :: TrackSpeed -> [Double]
xy TrackSpeed{time = PilotTime x, frac = SpeedFraction y} =
    [x, y]

timeRange :: [TrackSpeed] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackSpeed{time = PilotTime x} -> x) <$> xs

hgPlot
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
hgPlot sgs sEx tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-speed") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.hgPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                                return ())
                            ]

                        elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $ do
                                    el "li" $ do
                                        elClass "span" "legend-time" $ text "─"
                                        text " GAP equation"
                                    el "li" $ do
                                        elClass "span" "legend-time" $ text "- -"
                                        text " FS equation"

                        tm' <- sample . current $ tm

                    return ()

        elClass "div" "tile is-child" $ tablePilot sgs sEx tm

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
tablePilot sgs sEx xs = do
    let sEx' = Map.fromList <$> sEx
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "H.hhh"
                    el "th" $ text "HH:MM:SS"
                    elClass "th" "th-norm th-norm-pace" . dynText
                        $ ffor sgs (\case [] -> "✓-Pace"; _ -> "✓-Time")

                    elClass "th" "th-norm th-time-diff" $ dynText
                        $ ffor sgs (\case [] -> "Δ-Pace"; _ -> "Δ-Time")

                    el "th" $ text "Fraction"
                    elClass "th" "th-norm" $ text "✓-Fraction"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry (rowSpeed sEx') . splitDynPure)

    return ()

rowSpeed
    :: MonadWidget t m
    => Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t Pilot
    -> Dynamic t TrackSpeed
    -> m ()
rowSpeed sEx pilot tm = do
    (yEl, yElDiff, yFrac) <- sample . current
                $ ffor3 pilot sEx tm (\pilot' sEx' TrackSpeed{time = elap} ->
                case Map.lookup pilot' sEx' of
                    Just Norm.NormBreakdown {timeElapsed = elap', timeFrac = tf} ->
                        ( maybe "" showPilotTime elap'
                        , maybe "" (flip showPilotTimeDiff elap) elap'
                        , showFrac tf
                        )

                    _ -> ("", "", ""))

    el "tr" $ do
        el "td" . dynText $ showHr . time <$> tm
        el "td" . dynText $ showHms . time <$> tm
        elClass "td" "td-norm td-norm-pace" . text $ yEl
        elClass "td" "td-norm td-time-diff" . text $ yElDiff
        el "td" . dynText $ showFrac . frac <$> tm
        elClass "td" "td-norm" . text $ yFrac
        el "td" . dynText $ showPilotName <$> pilot

        return ()

showHr :: PilotTime -> T.Text
showHr (PilotTime x) = showHours x

showHms :: PilotTime -> T.Text
showHms (PilotTime x) = showHmsForHours x

showFrac :: SpeedFraction -> T.Text
showFrac (SpeedFraction x) = T.pack $ printf "%.3f" x

