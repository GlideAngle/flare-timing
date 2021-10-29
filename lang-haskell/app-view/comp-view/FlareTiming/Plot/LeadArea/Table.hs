{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadArea.Table (tablePilotArea) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead (TrackLead(..), showArea, showCoef)
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tablePilotArea
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotArea tweak xs select = do
    ev <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> do
            ePilot <- tablePilotSimple tweak xs select
            return ePilot

        _ -> do
            ePilot <- tablePilotCompare tweak xs select
            return ePilot)

    switchHold never ev

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

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ dynText thArea
                el "th" $ text "Coef"
                el "th" . dynText $ ffor w hashIdHyphenPilot

                return ()

        el "tbody" $ do
            ePilots <- simpleList xs (uncurry (rowLeadSimple w select) . splitDynPure)
            return $ switchDyn $ leftmost <$> ePilots

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
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCompare tweak xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    let thArea = ffor tweak thAreaUnits

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                elClass "th" "th-lead-area" $ dynText thArea
                el "th" $ text "Coef"
                el "th" . dynText $ ffor w hashIdHyphenPilot

        ePilots <- el "tbody" $ simpleList xs (uncurry (rowLeadCompare w select) . splitDynPure)
        return $ switchDyn $ leftmost <$> ePilots

rowLeadCompare
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadCompare w select p tl = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> tl
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
