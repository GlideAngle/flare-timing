{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadArea.View (leadAreaPlot) where

import Reflex.Dom
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.LeadArea.Plot as P (leadAreaPlot)

import WireTypes.Comp (Tweak(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Lead
    ( TrackLead(..), RawLeadingArea(..), EssTime(..)
    , LeadingAreas(..), LeadingAreaSquared(..)
    , nullArea, showAreaSquared
    )
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.LeadArea.Table (tablePilotArea)
import FlareTiming.Comms (getTaskPilotArea)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Plot.Event (tableClass, mkMsg, legendClasses, numLegendPilots)

xyRange :: [[Double]] -> ((Double, Double), (Double, Double))
xyRange xys =
    case (null xs, null ys) of
        (True, _) -> ((0, 1), (0, 1))
        (_, True) -> ((0, 1), (0, 1))
        (False, False) -> ((minimum xs, maximum xs), (minimum ys, maximum ys))
    where
        xs = concat $ (take 1) <$> xys
        ys = concat $ (take 1 . drop 1) <$> xys

seriesRangeOrDefault :: [RawLeadingArea] -> ((Double, Double), (Double, Double))
seriesRangeOrDefault [] = ((0, 1), (0, 1))
seriesRangeOrDefault xs = maximum $ seriesRange <$> xs

seriesRange :: RawLeadingArea -> ((Double, Double), (Double, Double))
seriesRange RawLeadingArea{leadAllDown, raceDistance, distanceTime = xs} =
    (xR', yR')
    where
        (xR, yR) = xyRange xs
        xR' = maybe xR (\(TaskDistance rd) -> (0, rd)) raceDistance
        yR' = maybe yR (\(EssTime down) -> (0, down)) leadAllDown

leadAreaPlot
    :: MonadWidget t m
    => IxTask
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
leadAreaPlot ix tweak sEx xs = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    let mkLegend classes (pp, areas) = when (pp /= nullPilot) $ do
            el "tr" $ do
                el "td" $ elClass "span" classes $ text "▩"
                el "td" . dynText $ ffor w (flip showPilot $ pp)
                case areas of
                    Nothing -> do
                        elAttr "td" ("colspan" =: "3") $ text ""
                        return ()

                    Just
                        LeadingAreas
                            { areaFlown = LeadingAreaSquared af
                            , areaAfterLanding = LeadingAreaSquared al
                            , areaBeforeStart = LeadingAreaSquared bs
                            }
                      | af == 0 && al == 0 && bs == 0 -> do
                        elAttr "td" ("colspan" =: "5") $ text ""
                        return ()

                    Just LeadingAreas{areaFlown = af, areaAfterLanding = al, areaBeforeStart = bs} -> do
                        let afl = af + al
                        let abfl = bs + afl

                        elClass "td" "has-text-right" . text $ showAreaSquared bs
                        elClass "td" "has-text-right has-text-weight-bold" . text $ showAreaSquared af
                        elClass "td" "has-text-right" . text $ showAreaSquared al
                        elClass "td" "has-text-right has-text-weight-bold" . text $ showAreaSquared afl
                        elClass "td" "has-text-right" . text $ showAreaSquared abfl

                        return ()

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile is-7" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    mkMsg dPilot "Tap a row to plot distance versus time and visualise area."

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ leftmost
                            [ ffor eAreas (\as -> liftIO $ do
                                _ <- P.leadAreaPlot
                                        (_element_raw elPlot)
                                        (seriesRangeOrDefault as)
                                        (distanceTime <$> as)
                                        (distanceTimeAfterLanding <$> as)
                                        (distanceTimeBeforeStart <$> as)

                                return ())
                            ]

                    elClass "div" "level" $
                            elClass "div" "level-item" $ do
                                _ <- elDynClass "table" (tableClass <$> dPilot) $
                                        el "thead" $ do
                                            el "tr" $ do
                                                el "th" $ text ""
                                                el "th" . dynText $ ffor w hashIdHyphenPilot
                                                elClass "th" "has-text-right" $ text "b = Before"
                                                elClass "th" "has-text-right" $ text "f = Flown"
                                                elClass "th" "has-text-right" $ text "a = After"
                                                elClass "th" "has-text-right" $ text "f + a"
                                                elClass "th" "has-text-right" $ text "b + f + a"

                                                return ()

                                            sequence_
                                                [ widgetHold (return ()) $ ffor e (mkLegend c)
                                                | c <- legendClasses
                                                | e <- [e1, e2, e3, e4, e5]
                                                ]

                                            return ()

                                return ()
                    return ()

        ePilot :: Event _ Pilot <- elClass "div" "tile is-child" $ tablePilotArea tweak sEx xs dPilots
        dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

        area :: Event _ RawLeadingArea <- getTaskPilotArea ix (updated dPilot)
        pilotArea :: Dynamic _ (Pilot, RawLeadingArea) <- holdDyn (nullPilot, nullArea) (attachPromptlyDyn dPilot area)
        pilotArea' :: Dynamic _ (Pilot, RawLeadingArea) <- holdUniqDyn pilotArea

        let pilotAreas :: [(Pilot, RawLeadingArea)] = take numLegendPilots $ repeat (nullPilot, nullArea)
        dPilotAreas :: Dynamic _ [(Pilot, RawLeadingArea)] <- foldDyn (\pa pas -> take numLegendPilots $ pa : pas) pilotAreas (updated pilotArea')
        let dPilots :: Dynamic _ [Pilot] = ffor dPilotAreas (fmap fst)
        let ePilotAreas :: Event _ [(Pilot, RawLeadingArea)] = updated dPilotAreas
        let es :: Event _ [(Pilot, Maybe _)] = ffor ePilotAreas ((fmap . fmap) (Just . areas))
        let eAreas :: Event _ [RawLeadingArea] = ffor ePilotAreas (fmap snd)

        let nth n = updated
                    <$> foldDyn
                            (\ps np ->
                                case take 1 . drop n $ (ps ++ repeat np) of
                                    p : _ -> p
                                    _ -> np)
                            (nullPilot, Nothing)
                            es

        -- TODO: Find out why I get "cyclic evaluation in fixIO" if I use this list
        -- rather than pattern matching on the elements of the list.
        [e1, e2, e3, e4, e5] <- sequence $ nth <$> [0 .. 4]

        return ()
    return ()
