module FlareTiming.Plot.Time.View (timePlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Time.Plot as P (timePlot)

import WireTypes.Fraction (SpeedFraction(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (StartGate)
import FlareTiming.Plot.Time.Table (tableSpeed)

placings :: [TrackSpeed] -> [[Double]]
placings = fmap xy

xy :: TrackSpeed -> [Double]
xy TrackSpeed{time = PilotTime x, frac = SpeedFraction y} =
    [x, y]

timeRange :: [TrackSpeed] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackSpeed{time = PilotTime x} -> x) <$> xs

timePlot
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
timePlot sgs sEx tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-speed") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.timePlot (_element_raw elPlot) (timeRange xs) (placings xs)
                                return ())
                            ]

                        elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $ do
                                    el "li" $ do
                                        elClass "span" "legend-time" $ text "─"
                                        text " GAP equation"
                                    el "li" $ do
                                        elClass "span" "legend-time" $ text "- -"
                                        text " FS equation"

                        tm' <- sample . current $ tm

                    return ()

        elClass "div" "tile is-child" $ tableSpeed sgs sEx tm

    return ()
