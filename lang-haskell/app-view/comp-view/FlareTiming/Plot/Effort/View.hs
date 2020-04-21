module FlareTiming.Plot.Effort.View (effortPlot) where

import Reflex.Dom
import Reflex.Time (delay)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Effort.Plot as P (effortPlot)

import WireTypes.Fraction (EffortFraction(..))
import WireTypes.Effort (TrackEffort(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Plot.Effort.Table (tableEffort)

placings :: [TrackEffort] -> [[Double]]
placings = fmap xy

xy :: TrackEffort -> [Double]
xy TrackEffort{effort = PilotDistance x, frac = EffortFraction y} =
    [x, y]

timeRange :: [TrackEffort] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackEffort{effort = PilotDistance x} -> x) <$> xs

effortPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackEffort)]
    -> m ()
effortPlot sEx tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent is-vertical" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-effort") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.effortPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                                return ())
                            ]

                        elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $
                                    el "li" $ do
                                        elClass "span" "legend-effort" $ text "- - -"
                                        text " line of constant effort"

                        tm' <- sample . current $ tm

                    return ()

        elClass "div" "tile is-child" $ tableEffort sEx tm

    return ()
