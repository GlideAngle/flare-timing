module FlareTiming.Plot.Time.View (timePlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Time.Plot as P (timePlot)

import WireTypes.Fraction (Fractions(..), SpeedFraction(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..))
import qualified WireTypes.Speed as Speed (TrackSpeed(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (StartGate, Points(..), TimePoints(..))
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

timePlot
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
timePlot sgs sEx tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-speed") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.timePlot (_element_raw elPlot) (timeRange xs) (placings xs)
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
    let pts = fmap ((\Points{time = TimePoints x} -> x) . Norm.breakdown . snd) <$> sEx
    let maxPts = ffor pts (\case [] -> 0; pts' -> maximum pts')
    let sEx' = Map.fromList <$> sEx
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "5") $ text ""
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-norm")
                        $ text "✓ Fraction of"

                    el "th" $ text ""

                el "tr" $ do
                    el "th" $ text "H.hhh"
                    el "th" $ text "HH:MM:SS"
                    elClass "th" "th-norm th-norm-pace" . dynText
                        $ ffor sgs (\case [] -> "✓-Pace"; _ -> "✓-Time")

                    elClass "th" "th-norm th-time-diff" $ dynText
                        $ ffor sgs (\case [] -> "Δ-Pace"; _ -> "Δ-Time")

                    el "th" $ text "Fraction"
                    elClass "th" "th-norm" $ text "Time"
                    elClass "th" "th-norm" $ text "Points"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry (rowSpeed maxPts sEx') . splitDynPure)

    return ()

rowSpeed
    :: MonadWidget t m
    => Dynamic t Double
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t Pilot
    -> Dynamic t TrackSpeed
    -> m ()
rowSpeed maxPts sEx pilot tm = do
    maxPts' <- sample . current $ maxPts
    (yEl, yElDiff, yFrac, pFrac) <- sample . current
                $ ffor3 pilot sEx tm (\pilot' sEx' TrackSpeed{time = elap} ->
                    case Map.lookup pilot' sEx' of
                        Just
                            Norm.NormBreakdown
                                { breakdown = Points{time = TimePoints pts}
                                , timeElapsed = elap'
                                , fractions = Fractions{time = tf}
                                } ->
                            ( maybe "" showPilotTime elap'
                            , maybe "" (flip showPilotTimeDiff elap) elap'
                            , showFrac tf
                            , showFrac . SpeedFraction $ pts / maxPts'
                            )

                        _ -> ("", "", "", ""))

    el "tr" $ do
        el "td" . dynText $ showHr . Speed.time <$> tm
        el "td" . dynText $ showHms . Speed.time <$> tm
        elClass "td" "td-norm td-norm-pace" . text $ yEl
        elClass "td" "td-norm td-time-diff" . text $ yElDiff
        el "td" . dynText $ showFrac . frac <$> tm
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ pFrac
        el "td" . dynText $ showPilotName <$> pilot

        return ()

showHr :: PilotTime -> T.Text
showHr (PilotTime x) = showHours x

showHms :: PilotTime -> T.Text
showHms (PilotTime x) = showHmsForHours x

showFrac :: SpeedFraction -> T.Text
showFrac (SpeedFraction x) = T.pack $ printf "%.3f" x
