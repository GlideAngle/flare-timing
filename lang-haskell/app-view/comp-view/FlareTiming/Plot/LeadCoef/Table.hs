{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.LeadCoef.Table (tablePilotCoef) where

import Reflex.Dom
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (Fractions(..), showLeadingFrac, showLeadingFracDiff)
import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead (TrackLead(..), showCoef, showCoefDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tablePilotCoef
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCoef tweak sEx xs select = do
    ev <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> do
            ePilot <- tablePilotSimple xs select
            return ePilot

        _ -> do
            ePilot <- tablePilotCompare tweak sEx xs select
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

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ showLeadingFrac . frac <$> av
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotCompare _ sEx xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3") $ text "Coefficient"
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-frac"))
                        $ text "Fraction"
                    el "th" . dynText $ ffor w hashIdHyphenPilot
                el "tr" $ do
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""

                    return ()

            ev <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    ePilots <- el "tbody" $
                        simpleList xs (uncurry (rowLeadCompare w mapN select) . splitDynPure)
                    let ePilot' = switchDyn $ leftmost <$> ePilots
                    return ePilot')

            return ev
    switchHold never ev

rowLeadCompare
    :: MonadWidget t m
    => Dynamic t Int
    -> Map.Map Pilot Norm.NormBreakdown
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadCompare w mapN select p tl = do
    (pilot, yCoef, yCoefDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p tl (\pilot TrackLead{coef, frac} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown
                                { leadingCoef = coef'
                                , fractions = Fractions{leading = frac'}
                                } ->
                            ( pilot
                            , showCoef coef'
                            , showCoefDiff coef' coef
                            , showLeadingFrac frac'
                            , showLeadingFracDiff frac' frac
                            )

                        _ -> (pilot, "", "", "", ""))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        elClass "td" "td-norm td-norm" . text $ yCoef
        elClass "td" "td-norm td-time-diff" . text $ yCoefDiff
        elClass "td" "td-lead-frac" . dynText $ showLeadingFrac . frac <$> tl
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
