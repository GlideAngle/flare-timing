{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadCoef.View (leadCoefPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Data.List (find)
import Data.Maybe (catMaybes)
import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.LeadCoef.Plot as P (leadCoefPlot)

import WireTypes.Fraction (LeadingFraction(..))
import WireTypes.Comp (Tweak(..))
import WireTypes.Lead (TrackLead(..), LeadingCoefficient(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.LeadCoef.Table (tablePilotCoef)
import FlareTiming.Events (IxTask(..))

placings :: [TrackLead] -> [[Double]]
placings = fmap xy

xy :: TrackLead -> [Double]
xy TrackLead{coef = LeadingCoefficient x, frac = LeadingFraction y} =
    [x, y]

lcRange :: [TrackLead] -> (Double, Double)
lcRange xs =
    (minimum ys, maximum ys)
    where
        ys = (\TrackLead{coef = LeadingCoefficient x} -> x) <$> xs

leadCoefPlot
    :: MonadWidget t m
    => IxTask
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
leadCoefPlot _ix tweak sEx xs = do
    pb <- delay 1 =<< getPostBuild
    let w = ffor xs (pilotIdsWidth . fmap fst)
    let pilotLegend classes pp = do
            el "td" $ elClass "span" classes $ text "▩"
            el "td" . dynText $ ffor w (flip showPilot $ pp)
            return ()

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile is-7" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    let dMsgClass = ffor dPilot (\p -> "message is-primary" <> if p == nullPilot then "" else " is-hidden")

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ ffor eRedraw (\ps -> liftIO $ do
                        let leads = snd . unzip $ ys
                        let leads' =
                                snd . unzip . catMaybes $
                                [ find (\(Pilot (qid, _), _) -> pid == qid) ys
                                | Pilot (pid, _) <- ps
                                ]

                        _ <- P.leadCoefPlot (_element_raw elPlot) (lcRange leads) (placings leads) (placings leads')
                        return ())

                    _ <- elDynClass "article" dMsgClass $ do
                            elClass "div" "message-header" $ do
                                el "p" $ text "Plot Instructions"
                            elClass "div" "message-body" $
                                text "Tap a row to highlight that pilot's point on the plot."

                            return ()

                    let dTableClass = ffor dPilot (\p -> "table is-striped" <> if p == nullPilot then " is-hidden" else "")
                    elClass "div" "level" $
                            elClass "div" "level-item" $ do
                                _ <- elDynClass "table" dTableClass $ do
                                        el "thead" $ do
                                            el "tr" $ do
                                                el "th" $ text ""
                                                el "th" . dynText $ ffor w hashIdHyphenPilot
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-reach" <$> ePilotLegend1
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-effort" <$> ePilotLegend2
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-time" <$> ePilotLegend3
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-leading" <$> ePilotLegend4
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-arrival" <$> ePilotLegend5
                                                return ()

                                        el "tfoot" $ do
                                            el "tr" $ do
                                                el "td" $ text "─"
                                                el "td" $ text "GAP Equation"
                                                return ()

                                            el "tr" $ do
                                                el "td" $ text "- -"
                                                el "td" $ text "FS equation"
                                                return ()

                                return ()
                    return ()

        ys <- sample $ current xs

        ePilot :: Event _ Pilot <- elClass "div" "tile is-child" $ tablePilotCoef tweak sEx xs dPilots
        dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

        let pilots :: [Pilot] = take 5 $ repeat nullPilot
        dPilots :: Dynamic _ [Pilot] <- foldDyn (\pa pas -> take 5 $ pa : pas) pilots (updated dPilot)
        let ePilots :: Event _ [Pilot] = updated dPilots
        let eRedraw = leftmost [[] <$ pb, ePilots]

        ePilotLegend1 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 $ ps ++ repeat np of
                            p : _ -> p
                            _ -> np)
                    nullPilot
                    ePilots

        ePilotLegend2 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 1 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    nullPilot
                    ePilots

        ePilotLegend3 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 2 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    nullPilot
                    ePilots

        ePilotLegend4 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 3 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    nullPilot
                    ePilots

        ePilotLegend5 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 4 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    nullPilot
                    ePilots

        return ()

    return ()
