{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.Time.View (timeViePlot) where

import Reflex.Dom
import Data.List (find)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified FlareTiming.Plot.Time.Plot as P (timePlot)

import WireTypes.Fraction (SpeedFraction(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (hashIdHyphenPilot)
import WireTypes.Point (StartGate)
import FlareTiming.Plot.Time.Table (tableSpeed)
import FlareTiming.Plot.Event
    (tableClass, mkMsg, mkLegend, legendClasses, numLegendPilots, selectPilots)

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

timeViePlot
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
timeViePlot sgs sEx xs = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    mkMsg dPilot "Tap a row to highlight that pilot's point on the plot."

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-speed") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ ffor eRedraw (\ps -> liftIO $ do
                        let times = snd . unzip $ ys
                        let times' =
                                snd . unzip . catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ys
                                | Pilot (pid, _) <- ps
                                ]

                        _ <- P.timePlot (_element_raw elPlot) (timeRange times) (placings times) (placings times')
                        return ())

                    elAttr "div" ("id" =: "legend-time" <> "class" =: "level") $
                            elClass "div" "level-item" $ do
                                _ <- elDynClass "table" (tableClass <$> dPilot) $ do
                                        el "thead" $ do
                                            el "tr" $ do
                                                el "th" $ text ""
                                                el "th" . dynText $ ffor w hashIdHyphenPilot
                                                return ()

                                            sequence_
                                                [ widgetHold (return ()) $ ffor e (mkLegend w c)
                                                | c <- legendClasses
                                                | e <- [e1, e2, e3, e4, e5]
                                                ]

                                            return ()

                                        el "tfoot" $ do
                                            el "tr" $ do
                                                el "td" $ text "─"
                                                el "td" $ text "GAP Equation"
                                                return ()

                                            el "tr" $ do
                                                el "td" $ text "--"
                                                el "td" $ text "FS equation"
                                                return ()

                                return ()
                    return ()

        ys <- sample $ current xs

        let pilots :: [Pilot] = take numLegendPilots $ repeat nullPilot
        dPilots :: Dynamic _ [Pilot] <- foldDyn (\pa pas -> take numLegendPilots $ pa : pas) pilots (updated dPilot)
        (dPilot, eRedraw, (e1, e2, e3, e4, e5))
            <- selectPilots dPilots (\dPilots' -> elClass "div" "tile is-child" $ tableSpeed sgs sEx xs dPilots')

        return ()

    return ()
