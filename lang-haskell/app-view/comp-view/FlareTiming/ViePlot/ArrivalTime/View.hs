{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.ArrivalTime.View (arrivalTimeViePlot) where

import Reflex.Dom
import Data.List (find)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (MonadIO(..), liftIO)

import WireTypes.Fraction (ArrivalFraction(..))
import WireTypes.Arrival (TrackArrival(..), ArrivalLag(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (hashIdHyphenPilot)
import qualified FlareTiming.Plot.ArrivalTime.Plot as P (hgPlotTime)
import FlareTiming.Plot.ArrivalTime.Table (tableArrivalTime)
import FlareTiming.Plot.Event
    (tableClass, mkMsg, mkLegend, legendClasses, numLegendPilots, selectPilots)

lagMax :: [TrackArrival] -> Double
lagMax arrivals = maximum $ [x | TrackArrival{lag = ArrivalLag x} <- arrivals]

lags :: [TrackArrival] -> [[Double]]
lags arrivals = xyLag <$> arrivals

xyLag :: TrackArrival -> [Double]
xyLag TrackArrival{lag = ArrivalLag x, frac = ArrivalFraction y} = [x, y]

arrivalTimeViePlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalTimeViePlot xs xsN = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    mkMsg dPilot "Tap a row to highlight that pilot's point on the plot."

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-time") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ ffor eRedraw (\ps -> liftIO $ do
                        let arrivals = snd $ unzip ys
                        let arrivals' =
                                snd . unzip . catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ys
                                | Pilot (pid, _) <- ps
                                ]

                        _ <- P.hgPlotTime (_element_raw elPlot) (lagMax arrivals) (lags arrivals) (lags arrivals')
                        return ())

                    elAttr "div" ("id" =: "legend-arrival-time" <> "class" =: "level") $
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

        ys <- sample $ current xs

        let pilots :: [Pilot] = take numLegendPilots $ repeat nullPilot
        dPilots :: Dynamic _ [Pilot] <- foldDyn (\pa pas -> take numLegendPilots $ pa : pas) pilots (updated dPilot)
        (dPilot, eRedraw, (e1, e2, e3, e4, e5))
            <- selectPilots dPilots (\dPilots' -> elClass "div" "tile is-child" $ tableArrivalTime xs xsN dPilots')

        return ()
    return ()
