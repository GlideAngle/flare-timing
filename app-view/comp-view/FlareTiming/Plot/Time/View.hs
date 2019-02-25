module FlareTiming.Plot.Time.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Time.Plot as P (hgPlot)

import WireTypes.Speed (TrackSpeed(..), SpeedFraction(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (PilotTime(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (showHmsForHours)

placings :: [TrackSpeed] -> [[Double]]
placings = fmap xy

xy :: TrackSpeed -> [Double]
xy TrackSpeed{time = PilotTime x, frac = SpeedFraction y} =
    [x, y]

timeRange :: [TrackSpeed] -> (Double, Double)
timeRange xs =
    (minimum ys, maximum ys)
    where
        -- TODO: Parse pilot time as an hour, not a string.
        ys = (\TrackSpeed{time = PilotTime x} -> x) <$> xs

hgPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
hgPlot tm = do
    pb <- delay 1 =<< getPostBuild

    elAttr "div" (("class" =: "level") <> ("style" =: "align-items:flex-start;")) $ do
        elClass "div" "level-left" $
            elClass "div" "level-item" $ do
                (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-speed") <> ("style" =: "height: 460px;width: 720px")) $ return ()
                rec performEvent_ $ leftmost
                        [ ffor pb (\_ -> liftIO $ do
                            let xs = snd . unzip $ tm'
                            _ <- P.hgPlot (_element_raw elPlot) (timeRange xs) (placings xs)
                            return ())
                        ]

                    tm' <- sample . current $ tm

                return ()

        elClass "div" "level-right" $
            elClass "div" "level-item" $ tablePilot tm

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Time"
                    el "th" $ text "Fraction"
                    el "th" $ text "Name"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowSpeed . splitDynPure)

    return ()

rowSpeed
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackSpeed
    -> m ()
rowSpeed p tm = do
    el "tr" $ do
        el "td" . dynText $ showTime . time <$> tm
        el "td" . dynText $ showFrac . frac <$> tm
        el "td" . dynText $ showPilotName <$> p

        return ()

showTime :: PilotTime -> T.Text
showTime (PilotTime x) = showHmsForHours x

showFrac :: SpeedFraction -> T.Text
showFrac (SpeedFraction x) = T.pack $ printf "%.3f" x

