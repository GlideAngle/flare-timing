module FlareTiming.Plot.Arrival.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Arrival.Plot as P (hgPlot)

import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..), ArrivalFraction(..))
import WireTypes.ValidityWorking (PilotsFlying(..))
import WireTypes.Point (GoalRatio(..), Allocation(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilotName)

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

    _ <- dyn $ ffor av (\case
            [] ->
                elClass "article" "tile is-child notification is-warning" $ do
                    elClass "p" "title" $ text "Arrivals"
                    el "p" $ text "No pilots made it to the end of the speed section. There are no arrivals"

            _ ->
                elClass "div" "level" $ do
                    elClass "div" "level-left" $
                        elClass "div" "level-item" $ do
                            (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-arrival") <> ("style" =: "height: 360px;width: 360px")) $ return ()
                            rec performEvent_ $ leftmost
                                    [ ffor pb (\_ -> liftIO $ do
                                        _ <- P.hgPlot (_element_raw elPlot) pf gr
                                        return ())
                                    ]

                            return ()

                    elClass "div" "level-right" $
                        elClass "div" "level-item" $ tablePilot av)

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

