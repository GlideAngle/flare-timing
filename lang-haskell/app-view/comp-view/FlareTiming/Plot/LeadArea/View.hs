module FlareTiming.Plot.LeadArea.View (leadAreaPlot) where

import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.LeadArea.Plot as P (leadAreaPlot)

import WireTypes.Comp (Tweak(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Lead (TrackLead(..), RawLeadingArea(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilot)
import FlareTiming.Plot.LeadArea.Table (tablePilotArea)
import FlareTiming.Comms (getTaskPilotArea)
import FlareTiming.Events (IxTask(..))

xyRange :: [[Double]] -> ((Double, Double), (Double, Double))
xyRange xys =
    case (null xs, null ys) of
        (True, _) -> ((0, 1), (0, 1))
        (_, True) -> ((0, 1), (0, 1))
        (False, False) -> ((minimum xs, maximum xs), (minimum ys, maximum ys))
    where
        xs = concat $ (take 1) <$> xys
        ys = concat $ (take 1 . drop 1) <$> xys

leadAreaPlot
    :: MonadWidget t m
    => IxTask
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
leadAreaPlot ix tweak sEx ld = do

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile is-5" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 640px;width: 480px")) $ return ()
                    performEvent_ $ leftmost
                            [ ffor area (\RawLeadingArea{raceDistance, distanceTime = xs} -> liftIO $ do
                                let (xR, yR) = xyRange xs
                                let xR' = maybe xR (\(TaskDistance rd) -> (0, rd)) raceDistance
                                _ <- P.leadAreaPlot (_element_raw elPlot) (xR', yR) xs
                                return ())
                            ]

                    elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $ do
                                    el "li" $ do
                                        elClass "span" "legend-leading" $ text "─"
                                        text " GAP equation"
                                    el "li" $ do
                                        elClass "span" "legend-leading" $ text "- -"
                                        text " FS equation"

                    _ <- widgetHold (el "span" $ text "Select a pilot from the table") $
                                (\pp -> el "span" $ text (showPilot pp))<$> ePilot

                    return ()

        ePilot <- elClass "div" "tile is-child" $ tablePilotArea tweak sEx ld
        area <- getTaskPilotArea ix ePilot

        return ()

    return ()
