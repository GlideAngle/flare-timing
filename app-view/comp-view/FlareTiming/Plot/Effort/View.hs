module FlareTiming.Plot.Effort.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Effort.Plot as P (hgPlot)

import WireTypes.Effort (TrackEffort(..), EffortFraction(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import FlareTiming.Pilot (showPilotName)

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

hgPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackEffort)]
    -> m ()
hgPlot tm = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent is-vertical" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-effort") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ tm'
                                _ <- P.hgPlot (_element_raw elPlot) (timeRange xs) (placings xs)
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

        elClass "div" "tile is-child" $ tablePilot tm

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackEffort)]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-effort" $ text "Effort (km)"
                    el "th" $ text "Fraction"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowEffort . splitDynPure)

    return ()

rowEffort
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackEffort
    -> m ()
rowEffort p tm = do
    el "tr" $ do
        elClass "td" "td-plot-effort" . dynText $ (showPilotDistance 1) . effort <$> tm
        el "td" . dynText $ showFrac . frac <$> tm
        el "td" . dynText $ showPilotName <$> p

        return ()

showFrac :: EffortFraction -> T.Text
showFrac (EffortFraction x) = T.pack $ printf "%.3f" x
