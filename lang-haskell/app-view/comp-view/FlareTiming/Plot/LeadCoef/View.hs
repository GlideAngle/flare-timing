{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadCoef.View (leadCoefPlot) where

import Reflex.Dom
import Reflex.Time (delay)
import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.LeadCoef.Plot as P (leadCoefPlot)

import WireTypes.Fraction (LeadingFraction(..))
import WireTypes.Comp (Tweak(..))
import WireTypes.Route (TaskDistance(..))
import WireTypes.Lead
    ( TrackLead(..), RawLeadingArea(..), EssTime(..)
    , LeadingAreas(..), LeadingAreaSquared(..)
    , LeadingCoefficient(..)
    , nullArea, showAreaSquared
    )
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..), nullPilot, pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.LeadCoef.Table (tablePilotCoef)
import FlareTiming.Comms (getTaskPilotArea)
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
leadCoefPlot ix tweak sEx xs = do
    pb <- delay 1 =<< getPostBuild
    let w = ffor xs (pilotIdsWidth . fmap fst)
    let pilotLegend classes (pp, _) = do
            el "td" $ elClass "span" classes $ text "▩"
            el "td" . dynText $ ffor w (flip showPilot $ pp)
            return ()

    elClass "div" "tile is-ancestor" $ mdo
        elClass "div" "tile is-7" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    let dMsgClass = ffor dPilot (\p -> "message is-primary" <> if p == nullPilot then "" else " is-hidden")

                    _ <- elDynClass "article" dMsgClass $ do
                            elClass "div" "message-header" $ do
                                el "p" $ text "Plot Instructions"
                            elClass "div" "message-body" $
                                text "Tap a row to plot distance versus time and visualise area."

                            return ()

                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 640px;width: 700px")) $ return ()
                    performEvent_ $ leftmost
                            [ ffor eAreas (\_ -> liftIO $ do
                                let ld = snd . unzip $ ld'
                                _ <- P.leadCoefPlot (_element_raw elPlot) (lcRange ld) (placings ld)
                                return ())
                            ]

                    let dTableClass = ffor dPilot (\p -> "table is-striped" <> if p == nullPilot then " is-hidden" else "")
                    elClass "div" "level" $
                            elClass "div" "level-item" $ do
                                _ <- elDynClass "table" dTableClass $
                                        el "thead" $ do
                                            el "tr" $ do
                                                el "th" $ text ""
                                                el "th" . dynText $ ffor w hashIdHyphenPilot

                                                return ()
                                            el "tr" $ do
                                                _ <- widgetHold (el "span" $ text "") $
                                                            pilotLegend "legend-reach" <$> ePilotLegend1
                                                return ()

                                return ()
                    return ()

        ld' <- sample $ current xs

        ePilot :: Event _ Pilot <- elClass "div" "tile is-child" $ tablePilotCoef tweak sEx xs dPilots
        dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

        area :: Event _ RawLeadingArea <- getTaskPilotArea ix (updated dPilot)
        pilotArea :: Dynamic _ (Pilot, RawLeadingArea) <- holdDyn (nullPilot, nullArea) (attachPromptlyDyn dPilot area)
        pilotArea' :: Dynamic _ (Pilot, RawLeadingArea) <- holdUniqDyn pilotArea

        let pilotAreas :: [(Pilot, RawLeadingArea)] = take 1 $ repeat (nullPilot, nullArea)
        dPilotAreas :: Dynamic _ [(Pilot, RawLeadingArea)] <- foldDyn (\pa pas -> take 1 $ pa : pas) pilotAreas (updated pilotArea')
        let dPilots :: Dynamic _ [Pilot] = ffor dPilotAreas (fmap fst)
        let ePilotAreas :: Event _ [(Pilot, RawLeadingArea)] = updated dPilotAreas
        let ePilotLegends :: Event _ [(Pilot, Maybe _)] = ffor ePilotAreas ((fmap . fmap) (Just . areas))
        let eAreas :: Event _ [RawLeadingArea] = ffor ePilotAreas (fmap snd)

        ePilotLegend1 <-
            updated
            <$> foldDyn
                    (\ps np ->
                        case take 1 $ ps ++ repeat np of
                            p : _ -> p
                            _ -> np)
                    (nullPilot, Nothing)
                    ePilotLegends

        return ()
    return ()
