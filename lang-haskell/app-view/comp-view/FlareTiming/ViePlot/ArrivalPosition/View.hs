{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.ArrivalPosition.View (arrivalPositionViePlot) where

import Reflex.Dom
import Data.List (find, partition)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (MonadIO(..), liftIO)

import WireTypes.Fraction (ArrivalFraction(..))
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (hashIdHyphenPilot)
import FlareTiming.ViePlot.ArrivalPosition.Table (tableVieArrivalPosition)
import qualified FlareTiming.ViePlot.ArrivalPosition.Plot as P (hgViePlotPosition)
import FlareTiming.Plot.Event
    (tableClass, mkMsg, mkLegend, legendClasses, numLegendPilots, selectPilots)

placings :: [TrackArrival] -> ([[Double]], [[Double]])
placings arrivals =
    (xyPosition <$> soloPlaces, xyPosition <$> equalPlaces)
    where
        (soloPlaces, equalPlaces) =
                partition
                    (\case
                        TrackArrival{rank = ArrivalPlacing _} -> True
                        TrackArrival{rank = ArrivalPlacingEqual _ _} -> False)
                    arrivals

xyPosition :: TrackArrival -> [Double]
xyPosition TrackArrival{rank = ArrivalPlacing x, frac = ArrivalFraction y} =
    [fromIntegral x, y]
xyPosition TrackArrival{rank = ArrivalPlacingEqual x _, frac = ArrivalFraction y} =
    [fromIntegral x, y]

arrivalPositionViePlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalPositionViePlot xs xsN = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    mkMsg dPilot "Tap a row to highlight that pilot's point on the plot."

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-position") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ ffor eRedraw (\ps -> liftIO $ do
                        let times = snd . unzip $ ys
                        let times' =
                                snd . unzip . catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ys
                                | Pilot (pid, _) <- ps
                                ]

                        _ <- P.hgViePlotPosition (_element_raw elPlot) (placings times) (placings times')
                        return ())

                    elAttr "div" ("id" =: "legend-arrival-position" <> "class" =: "level") $
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
            <- selectPilots dPilots (\dPilots' -> elClass "div" "tile is-child" $ tableVieArrivalPosition xs xsN dPilots')

        return ()
    return ()
