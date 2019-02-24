module FlareTiming.Plot.Arrival.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.List (partition)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Arrival.Plot as P (hgPlot)

import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..), ArrivalFraction(..))
import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (GoalRatio(..), Allocation(..))
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

hgPlot
    :: MonadWidget t m
    => Maybe PilotsFlying
    -> Maybe Allocation
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
hgPlot Nothing _ _ = return ()
hgPlot (Just pf) alloc av = do
    let gr = maybe (GoalRatio 0) goalRatio alloc
    pb <- delay 1 =<< getPostBuild

    elAttr "div" (("class" =: "level") <> ("style" =: "align-items:flex-start;")) $ do
        elClass "div" "level-left" $
            elClass "div" "level-item" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let (soloPlaces, equalPlaces) = placings . snd . unzip $ av'
                            _ <- P.hgPlot (_element_raw elPlot) pf gr soloPlaces equalPlaces
                            return ())
                        ]

                    av' <- sample . current $ av

                return ()

        elClass "div" "level-right" $
            elClass "div" "level-item" $ tablePilot av

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    el "th" $ text "Fraction"
                    el "th" $ text "Name"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowArrival . splitDynPure)

    return ()

rowArrival
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m ()
rowArrival p av = do
    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> av
        el "td" . dynText $ showFrac . frac <$> av
        el "td" . dynText $ showPilotName <$> p

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="

showFrac :: ArrivalFraction -> T.Text
showFrac (ArrivalFraction x) = T.pack $ printf "%.3f" x
