{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.LeadArea.Table (tableViePilotArea) where

import Reflex.Dom
import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as Map

import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead (TrackLead(..), showArea, showAreaDiff, showCoef)
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableViePilotArea
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableViePilotArea tweak sEx xs select = do
    ev <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> do
            ePilot <- tableViePilotSimple tweak xs select
            return ePilot

        _ -> do
            ePilot <- tableViePilotCompare tweak sEx xs select
            return ePilot)

    switchHold never ev

thAreaUnits :: Maybe Tweak -> T.Text
thAreaUnits tweak =
    "Area " <>
    if maybe True leadingAreaDistanceSquared tweak then "(km^2 s)" else "(km s)"

tableViePilotSimple
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableViePilotSimple tweak xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    let thArea = ffor tweak thAreaUnits

    ePilot :: Event _ Pilot <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ dynText thArea
                    el "th" $ text "Coef"
                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            ev <- el "tbody" $ do
                ePilots <- simpleList xs (uncurry (rowLeadSimple w select) . splitDynPure)
                return $ switchDyn $ leftmost <$> ePilots

            return ev
    return ePilot

rowLeadSimple
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadSimple w select p av = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showArea . area <$> av
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

tableViePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableViePilotCompare tweak sEx xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    let thArea = ffor tweak thAreaUnits

    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-area"))
                        $ dynText thArea
                    el "th" $ text "Coef"
                    el "th" . dynText $ ffor w hashIdHyphenPilot
                el "tr" $ do
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""
                    el "th" $ text ""

                    return ()

            ev <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowLeadCompare w mapN select) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

            el "tfoot" $
                el "tr" $
                    elAttr "td" ("colspan" =: "5") $
                        text "Δ A difference of actual value over expected value minus one."

            return ev

    switchHold never ev

rowLeadCompare
    :: MonadWidget t m
    => Dynamic t Int
    -> Map.Map Pilot Alt.AltBreakdown
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadCompare w mapN select p tl = do
    (pilot, yArea, yAreaDiff) <- sample . current
                $ ffor2 p tl (\pilot TrackLead{area} ->
                    case Map.lookup pilot mapN of
                        Just
                            Alt.AltBreakdown{leadingArea = area'} ->
                            ( pilot
                            , showArea area'
                            , showAreaDiff area' area
                            )

                        _ -> (pilot, "", ""))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> tl
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
