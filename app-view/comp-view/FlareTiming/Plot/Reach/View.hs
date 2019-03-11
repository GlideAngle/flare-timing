module FlareTiming.Plot.Reach.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Reach.Plot as P (hgPlot)

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

hgPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> m ()
hgPlot tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-reach") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let xs = snd . unzip $ tm'
                            _ <- P.hgPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                            return ())
                        ]

                    tm' <- sample . current $ tm

                return ()

        elClass "div" "tile is-child" $ tablePilot tm

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Reach (km)"
                    el "th" $ text "Fraction"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowReach . splitDynPure)

    return ()

rowReach
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReach p tm = do
    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance . reach <$> tm
        el "td" . dynText $ showFrac . frac <$> tm
        el "td" . dynText $ showPilotName <$> p

        return ()

showFrac :: ReachFraction -> T.Text
showFrac (ReachFraction x) = T.pack $ printf "%.3f" x
