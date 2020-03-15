{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadArea.View (leadAreaPlot) where

import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.LeadArea.Plot as P (leadAreaPlot)

import WireTypes.Comp (Tweak(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Lead
    ( TrackLead(..), RawLeadingArea(..), EssTime(..), LeadingAreas(..)
    , nullArea, showAreaSquared
    )
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..), nullPilot)
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
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
leadAreaPlot ix tweak sEx ld = do
    let pilotLegend classes (pp, areas) = do
            el "td" $ elClass "span" classes $ text "▩"
            el "td" $ text (showPilot pp)
            case areas of
                Nothing -> do
                    elAttr "td" ("colspan" =: "3") $ text ""
                    return ()

                Just LeadingAreas{areaFlown = af, areaAfterLanding = al, areaBeforeStart = bs} -> do
                    elClass "td" "has-text-right" . text $ showAreaSquared bs
                    elClass "td" "has-text-right" . text $ showAreaSquared af
                    elClass "td" "has-text-right" . text $ showAreaSquared al

                    return ()
            return ()

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile is-7" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
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

                    let dMsgClass = ffor dPilot (\p -> "message is-primary" <> if p == nullPilot then "" else " is-hidden")
                    let dTableClass = ffor dPilot (\p -> "table is-striped" <> if p == nullPilot then " is-hidden" else "")

                    elClass "div" "level" $
                            elClass "div" "level-item" $ do
                                _ <- elDynClass "article" dMsgClass $ do
                                        elClass "div" "message-header" $ do
                                            el "p" $ text "Plot Instructions"
                                        elClass "div" "message-body" $ text "Select a pilot from the table to see a plot of area"
                                        return ()

                                _ <- elDynClass "table" dTableClass $ do
                                        el "thead" $ do
                                            el "tr" $ do
                                                el "th" $ text ""
                                                el "th" $ text "###-Pilot"
                                                elClass "th" "has-text-right" $ text "Before"
                                                elClass "th" "has-text-right" $ text "Flown"
                                                elClass "th" "has-text-right" $ text "After"

                                                return ()
                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-reach" <$> ePilot1
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-effort" <$> ePilot2
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-time" <$> ePilot3
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-leading" <$> ePilot4
                                                return ()

                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-arrival" <$> ePilot5
                                                return ()
                                return ()
                    return ()

        ePilot :: Event _ Pilot <- elClass "div" "tile is-child" $ tablePilotArea tweak sEx ld
        dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

        area :: Event _ RawLeadingArea <- getTaskPilotArea ix (updated dPilot)
        pilotArea :: Dynamic _ (Pilot, RawLeadingArea) <- holdDyn (nullPilot, nullArea) (attachPromptlyDyn dPilot area)
        pilotArea' :: Dynamic _ (Pilot, RawLeadingArea) <- holdUniqDyn pilotArea

        let pilotAreas :: [(Pilot, RawLeadingArea)] = take 5 $ repeat (nullPilot, nullArea)
        dPilotAreas :: Dynamic _ [(Pilot, RawLeadingArea)] <- foldDyn (\pa pas -> take 5 $ pa : pas) pilotAreas (updated pilotArea')
        let ePilotAreas :: Event _ [(Pilot, RawLeadingArea)] = updated dPilotAreas
        let ePilots :: Event _ [(Pilot, Maybe _)] = ffor ePilotAreas ((fmap . fmap) (Just . areas))
        let eAreas :: Event _ [RawLeadingArea] = ffor ePilotAreas (fmap snd)

        ePilot1 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 $ ps ++ repeat np of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilots

        ePilot2 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 1 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilots

        ePilot3 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 2 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilots

        ePilot4 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 3 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilots

        ePilot5 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 . drop 4 $ (ps ++ repeat np) of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilots

        return ()

    return ()
