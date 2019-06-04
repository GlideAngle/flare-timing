module FlareTiming.Plot.Reach.View (reachPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (reachPlot)

import WireTypes.Reach (TrackReach(..), ReachFraction(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import FlareTiming.Pilot (showPilotName)

placings :: [TrackReach] -> [[Double]]
placings = fmap xy

xy :: TrackReach -> [Double]
xy TrackReach{reach = PilotDistance x, frac = ReachFraction y} =
    [x, y]

timeRange :: [TrackReach] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackReach{reach = PilotDistance x} -> x) <$> xs

reachPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
reachPlot reach bonusReach = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-reach") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let xs = snd . unzip $ reach'
                            _ <- P.reachPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                            return ())
                        ]

                    reach' <- sample . current $ reach

                return ()

        elClass "div" "tile is-child" $ tablePilotReach reach bonusReach

    return ()

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReach reach bonusReach = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Reach (km)"
                    elClass "th" "th-plot-reach" $ text "Bonus Reach (km)"
                    el "th" $ text "Fraction"
                    el "th" $ text "Pilot"

                    return ()

            _ <- dyn $ ffor bonusReach (\br -> do
                    let mapR = Map.fromList br

                    el "tbody" $
                        simpleList reach (uncurry (rowReach mapR) . splitDynPure))

            return ()
    return ()

rowReach
    :: MonadWidget t m
    => Map Pilot TrackReach
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReach mapR p r = do
    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r

        _ <- dyn $ ffor p (\p' ->
                elClass "td" "td-plot-reach" $
                    case Map.lookup p' mapR of
                        Nothing -> text ""
                        Just br -> text . (showPilotDistance 1) . reach $ br)

        el "td" . dynText $ showFrac . frac <$> r
        el "td" . dynText $ showPilotName <$> p

        return ()

showFrac :: ReachFraction -> T.Text
showFrac (ReachFraction x) = T.pack $ printf "%.3f" x
