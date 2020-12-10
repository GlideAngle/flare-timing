{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.Reach.View (reachViePlot) where

import Reflex.Dom
import Data.Maybe (isJust, catMaybes)
import Data.List (find)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (reachPlot)

import WireTypes.Fraction (ReachFraction(..))
import WireTypes.Comp (Task(..))
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import WireTypes.Point (PilotDistance(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import FlareTiming.Pilot (hashIdHyphenPilot)
import FlareTiming.Plot.Reach.TableReach (tablePilotReach)
import FlareTiming.Plot.Reach.TableBonus (tablePilotReachBonus)
import FlareTiming.Plot.Event
    (tableClass, mkMsg, mkLegend, legendClasses, numLegendPilots, selectPilots)

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
    | (_, TrackReach{reach = PilotDistance x}) <- pxs
    | (_, TrackReach{frac = ReachFraction y}) <- pys
    ]

reachViePlot
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
reachViePlot task sEx xs xsBonus = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    mkMsg dPilot "Tap a row to highlight that pilot's point on the plot."

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-reach") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ ffor eRedraw (\ps -> liftIO $ do
                        let ys' =
                                catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ys
                                | Pilot (pid, _) <- ps
                                ]

                        let ysBonus' =
                                catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ysBonus
                                | Pilot (pid, _) <- ps
                                ]

                        let reaches = snd . unzip $ ys
                        let reachesBonus = snd . unzip $ ysBonus

                        let reaches' = snd . unzip $ ys'
                        let reachesBonus' = snd . unzip $ ysBonus'

                        let tt = timeRange reachesBonus

                        _ <-
                            if isJust stopped then
                                P.reachPlot
                                    (_element_raw elPlot)
                                    tt
                                    (placings reachesBonus)
                                    (reValue ys ysBonus)
                                    (placings reachesBonus')
                                    (reValue ys' ysBonus')
                            else
                                P.reachPlot
                                    (_element_raw elPlot)
                                    tt
                                    (placings reaches)
                                    []
                                    (placings reaches')
                                    []
                        return ())

                    elAttr "div" ("id" =: "legend-effort" <> "class" =: "level") $
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
                                return ()
                    return ()

        Task{stopped} <- sample . current $ task
        ys <- sample $ current xs
        ysBonus <- sample . current $ xsBonus

        let pilots :: [Pilot] = take numLegendPilots $ repeat nullPilot
        dPilots :: Dynamic _ [Pilot] <- foldDyn (\pa pas -> take numLegendPilots $ pa : pas) pilots (updated dPilot)
        (dPilot, eRedraw, (e1, e2, e3, e4, e5))
            <- selectPilots dPilots (\dPilots' ->
                    elClass "div" "tile is-child" $ do
                        ev <- dyn $ ffor task (\case
                                Task{stopped = Nothing} -> tablePilotReach sEx xs dPilots'
                                Task{stopped = Just _} -> tablePilotReachBonus sEx xs xsBonus dPilots')
                        ePilot <- switchHold never ev
                        return ePilot)

        return ()

    return ()
