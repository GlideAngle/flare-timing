module FlareTiming.Plot.ArrivalTime.View (arrivalTimePlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Control.Monad.IO.Class (liftIO)

import WireTypes.Fraction (ArrivalFraction(..))
import WireTypes.Arrival (TrackArrival(..), ArrivalLag(..))
import WireTypes.Pilot (Pilot)
import qualified FlareTiming.Plot.ArrivalTime.Plot as P (hgPlotTime)
import FlareTiming.Plot.ArrivalTime.Table (tableArrivalTime)

lagMax :: [TrackArrival] -> Double
lagMax arrivals = maximum $ [x | TrackArrival{lag = ArrivalLag x} <- arrivals]

lags :: [TrackArrival] -> [[Double]]
lags arrivals = xyLag <$> arrivals

xyLag :: TrackArrival -> [Double]
xyLag TrackArrival{lag = ArrivalLag x, frac = ArrivalFraction y} = [x, y]

arrivalTimePlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalTimePlot xs xsN = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-time") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let arrivals = snd $ unzip xs'
                                let xys = lags arrivals
                                let lagMax' = lagMax arrivals
                                _ <- P.hgPlotTime (_element_raw elPlot) lagMax' xys
                                return ())
                            ]

                        xs' <- sample . current $ xs

                    return ()

        elClass "div" "tile is-child" $ tableArrivalTime xs xsN

    return ()
