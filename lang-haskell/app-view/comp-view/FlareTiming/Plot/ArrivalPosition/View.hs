module FlareTiming.Plot.ArrivalPosition.View (arrivalPositionPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Data.List (partition)
import Control.Monad.IO.Class (liftIO)

import WireTypes.Fraction (ArrivalFraction(..))
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..))
import qualified FlareTiming.Plot.ArrivalPosition.Plot as P (hgPlotPosition)
import FlareTiming.Plot.ArrivalPosition.Table (tableArrivalPosition)

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

arrivalPositionPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalPositionPlot xs xsN = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-position") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let (soloPlaces, equalPlaces) = placings . snd . unzip $ xs'
                                _ <- P.hgPlotPosition (_element_raw elPlot) soloPlaces equalPlaces
                                return ())
                            ]

                        xs' <- sample . current $ xs

                    return ()

        elClass "div" "tile is-child" $ tableArrivalPosition xs xsN
    return ()
