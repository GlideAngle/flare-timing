module FlareTiming.Plot.Arrival.View (arrivalPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.List (partition)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Arrival.Plot as P (hgPlot)

import WireTypes.Fraction (Fractions(..), ArrivalFraction(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilotName)

placings :: [TrackArrival] -> ([[Double]], [[Double]])
placings arrivals =
    (xy <$> soloPlaces, xy <$> equalPlaces)
    where
        (soloPlaces, equalPlaces) =
                partition
                    (\case
                        TrackArrival{rank = ArrivalPlacing _} -> True
                        TrackArrival{rank = ArrivalPlacingEqual _ _} -> False)
                    arrivals

xy :: TrackArrival -> [Double]
xy TrackArrival{rank = ArrivalPlacing x, frac = ArrivalFraction y} =
    [fromIntegral x, y]
xy TrackArrival{rank = ArrivalPlacingEqual x _, frac = ArrivalFraction y} =
    [fromIntegral x, y]

arrivalPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
arrivalPlot sEx av = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let (soloPlaces, equalPlaces) = placings . snd . unzip $ av'
                                _ <- P.hgPlot (_element_raw elPlot) soloPlaces equalPlaces
                                return ())
                            ]

                        av' <- sample . current $ av

                    return ()

        elClass "div" "tile is-child" $ tablePilot sEx av

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tablePilot sEx xs = do
    let sEx' = Map.fromList <$> sEx
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "2") $ text ""
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-norm")
                        $ text "Fraction"

                    el "th" $ text ""

                el "tr" $ do
                    el "th" $ text "#"
                    el "th" $ text "Fraction"
                    elClass "th" "th-norm th-norm-arrival" $ text "✓"
                    elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry (rowArrival sEx') . splitDynPure)

    return ()

rowArrival
    :: MonadWidget t m
    => Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m ()
rowArrival sEx p av = do
    (yFrac, diffFrac) <- sample . current
                $ ffor3 p sEx av (\p' sEx' TrackArrival{frac = f} ->
                    case Map.lookup p' sEx' of
                        Just Norm.NormBreakdown {fractions = Fractions{arrival = f'}} ->
                            ( showFrac f', showFracDiff f' f)

                        _ -> ("", ""))

    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> av
        el "td" . dynText $ showFrac . frac <$> av
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ diffFrac
        el "td" . dynText $ showPilotName <$> p

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="

showFrac :: ArrivalFraction -> T.Text
showFrac (ArrivalFraction x) = T.pack $ printf "%.3f" x

showFracDiff :: ArrivalFraction -> ArrivalFraction -> T.Text
showFracDiff (ArrivalFraction expected) (ArrivalFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"
