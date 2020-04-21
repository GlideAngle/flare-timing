module FlareTiming.Plot.ArrivalPosition.View (arrivalPositionPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)

import WireTypes.Fraction (ArrivalFraction(..), showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import qualified FlareTiming.Plot.ArrivalPosition.Plot as P (hgPlotPosition)

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
    let w = ffor xs (pilotIdsWidth . fmap fst)
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

        elClass "div" "tile is-child" $
            elClass "table" "table is-striped" $ do
                el "thead" $
                    el "tr" $ do
                        el "th" $ text "#"
                        el "th" $ text "Fraction"
                        elClass "th" "th-norm th-norm-arrival" $ text "✓"
                        elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                        el "th" . dynText $ ffor w hashIdHyphenPilot

                        return ()

                tableBody rowArrivalPosition xs xsN

                return ()
    return ()

type ShowRow t m
    = Dynamic t Int
    -> Map.Map Pilot TrackArrival
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
    let w = ffor xs (pilotIdsWidth . fmap fst)
    el "tbody" $ do
        _ <- dyn $ ffor xsN (\xsN' -> do
                let mapT = Map.fromList xsN'

                simpleList xs (uncurry (showRow w mapT) . splitDynPure))

        return ()
    return ()

rowArrivalPosition :: MonadWidget t m => ShowRow t m
rowArrivalPosition w mapT p ta = do
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
        el "td" . dynText $ ffor2 w p showPilot

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="
