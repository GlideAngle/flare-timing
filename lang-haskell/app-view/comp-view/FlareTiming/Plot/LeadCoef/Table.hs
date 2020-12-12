{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadCoef.Table (tablePilotCoef) where

import Reflex.Dom

import WireTypes.Fraction (showLeadingFrac)
import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead (TrackLead(..), showCoef)
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tablePilotCoef
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCoef tweak xs select = do
    ev <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> do
            ePilot <- tablePilotSimple xs select
            return ePilot

        _ -> do
            ePilot <- tablePilotCompare tweak xs select
            return ePilot)

    switchHold never ev

tablePilotSimple
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotSimple xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    ePilot :: Event _ Pilot <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Coef"
                    el "th" $ text "Frac"
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
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ showLeadingFrac . frac <$> av
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCompare _ xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text "Coefficient"
                elClass "th" "th-lead-frac" $ text "Fraction"
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
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        elClass "td" "td-lead-frac" . dynText $ showLeadingFrac . frac <$> tl
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
