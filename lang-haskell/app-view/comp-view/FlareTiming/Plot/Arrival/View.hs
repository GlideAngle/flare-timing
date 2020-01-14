module FlareTiming.Plot.Arrival.View (arrivalPositionPlot, arrivalTimePlot) where

import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.List (partition)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Arrival.PlotPosition as P (hgPlotPosition)
import qualified FlareTiming.Plot.Arrival.PlotTime as P (hgPlotTime)

import WireTypes.Fraction (ArrivalFraction(..), showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival
    ( TrackArrival(..), ArrivalPlacing(..), ArrivalLag(..)
    , showArrivalLag, showArrivalLagDiff)
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilot)

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

lagMax :: [TrackArrival] -> Double
lagMax arrivals = maximum $ [x | TrackArrival{lag = ArrivalLag x} <- arrivals]

lags :: [TrackArrival] -> [[Double]]
lags arrivals = xyLag <$> arrivals

xyLag :: TrackArrival -> [Double]
xyLag TrackArrival{lag = ArrivalLag x, frac = ArrivalFraction y} =
    [x, y]

arrivalPositionPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalPositionPlot av avN = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-position") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let (soloPlaces, equalPlaces) = placings . snd . unzip $ av'
                                _ <- P.hgPlotPosition (_element_raw elPlot) soloPlaces equalPlaces
                                return ())
                            ]

                        av' <- sample . current $ av

                    return ()

        elClass "div" "tile is-child" $
            elClass "table" "table is-striped" $ do
                el "thead" $
                    el "tr" $ do
                        el "th" $ text "#"
                        el "th" $ text "Fraction"
                        elClass "th" "th-norm th-norm-arrival" $ text "✓"
                        elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                        el "th" $ text "###-Pilot"

                        return ()

                tableBody rowArrivalPosition av avN

                return ()
    return ()

arrivalTimePlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalTimePlot av avN = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival-time") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let arrivals = snd $ unzip av'
                                let xys = lags arrivals
                                let lagMax' = lagMax arrivals
                                _ <- P.hgPlotTime (_element_raw elPlot) lagMax' xys
                                return ())
                            ]

                        av' <- sample . current $ av

                    return ()

        elClass "div" "tile is-child" $
            elClass "table" "table is-striped" $ do
                el "thead" $
                    el "tr" $ do
                        el "th" $ text "#"
                        el "th" $ text "Lag"
                        elClass "th" "th-norm th-norm-arrival" $ text "✓"
                        elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                        el "th" $ text "Fraction"
                        elClass "th" "th-norm th-norm-arrival" $ text "✓"
                        elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                        el "th" $ text "###-Pilot"

                        return ()

                tableBody rowArrivalTime av avN

                return ()

    return ()

type ShowRow t m
    = Map.Map Pilot TrackArrival
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m ()

tableBody
    :: MonadWidget t m
    => ShowRow t m
    -> Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tableBody showRow xs xsN = do
    el "tbody" $ do
        _ <- dyn $ ffor xsN (\xsN' -> do
                let mapT = Map.fromList xsN'

                simpleList xs (uncurry (showRow mapT) . splitDynPure))

        return ()
    return ()

rowArrivalTime :: MonadWidget t m => ShowRow t m
rowArrivalTime mapT p ta = do
    (yLag, yLagDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\pilot TrackArrival{frac, lag} ->
                    case Map.lookup pilot mapT of
                        Just TrackArrival{frac = fracN, lag = lagN} ->
                            ( showArrivalLag lagN
                            , showArrivalLagDiff lagN lag
                            , showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", "", "", ""))

    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showArrivalLag . lag <$> ta
        elClass "td" "td-norm" . text $ yLag
        elClass "td" "td-norm" . text $ yLagDiff
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ showPilot <$> p

        return ()

rowArrivalPosition :: MonadWidget t m => ShowRow t m
rowArrivalPosition mapT p ta = do
    (yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\pilot TrackArrival{frac} ->
                    case Map.lookup pilot mapT of
                        Just TrackArrival{frac = fracN} ->
                            ( showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", ""))

    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ showPilot <$> p

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="
