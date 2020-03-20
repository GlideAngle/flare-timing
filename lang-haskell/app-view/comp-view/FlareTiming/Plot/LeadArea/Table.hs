{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadArea.Table (tablePilotArea) where

import Reflex.Dom
import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as Map

import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead (TrackLead(..), showArea, showAreaDiff, showCoef)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)

tablePilotArea
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotArea tweak sEx xs select = do
    ev <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> do
            ePilot <- tablePilotSimple tweak xs select
            return ePilot

        _ -> do
            ePilot <- tablePilotCompare tweak sEx xs select
            return ePilot)

    ePilot <- switchHold never ev
    return ePilot

thAreaUnits :: Maybe Tweak -> T.Text
thAreaUnits tweak =
    "Area " <>
    if maybe True leadingAreaDistanceSquared tweak then "(km^2 s)" else "(km s)"

tablePilotSimple
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotSimple tweak xs select = do
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
                let ePilot' = switchDyn $ leftmost <$> ePilots
                return ePilot'

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
    let rowClass = ffor2 p select (\p' ps -> if p' `elem` ps then "is-selected" else "")

    (eRow, _) <- elDynClass' "tr" rowClass $ do
        el "td" . dynText $ showArea . area <$> av
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    let ePilot = const pilot <$> domEvent Click eRow
    return ePilot

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCompare tweak sEx xs select = do
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

                    ePilots <- el "tbody" $
                            simpleList xs (uncurry (rowLeadCompare w mapN select) . splitDynPure)
                    let ePilot' = switchDyn $ leftmost <$> ePilots
                    return ePilot')

            el "tfoot" $
                el "tr" $
                    elAttr "td" ("colspan" =: "5") $
                        text "Δ A difference of actual value over expected value minus one."

            return ev

    ePilot <- switchHold never ev
    return ePilot

rowLeadCompare
    :: MonadWidget t m
    => Dynamic t Int
    -> Map.Map Pilot Norm.NormBreakdown
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadCompare w mapN select p tl = do
    (pilot, yArea, yAreaDiff) <- sample . current
                $ ffor2 p tl (\pilot TrackLead{area} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown{leadingArea = area'} ->
                            ( pilot
                            , showArea area'
                            , showAreaDiff area' area
                            )

                        _ -> (pilot, "", ""))

    let rowClass = ffor2 p select (\p' ps -> if p' `elem` ps then "is-selected" else "")

    (eRow, _) <- elDynClass' "tr" rowClass $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> tl
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    let ePilot = const pilot <$> domEvent Click eRow
    return ePilot
