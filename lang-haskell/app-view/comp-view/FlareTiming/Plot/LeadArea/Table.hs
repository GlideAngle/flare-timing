module FlareTiming.Plot.LeadArea.Table (tablePilotArea) where

import Reflex.Dom
import qualified Data.Map.Strict as Map
import qualified Data.Text as T (unpack)

import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead
    ( TrackLead(..)
    , showArea, showAreaDiff
    , showCoef, showCoefDiff
    )
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilot)

tablePilotArea
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilotArea tweak sEx xs = do
    _ <- dyn $ ffor tweak (\case
        Just Tweak{leadingWeightScaling = Just (LwScaling 0)} -> tablePilotSimple xs
        _ -> tablePilotCompare tweak sEx xs)

    return ()

tablePilotSimple
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilotSimple xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Area"
                    el "th" $ text "Coef"
                    el "th" $ text "###-Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowLeadSimple . splitDynPure)

    return ()

rowLeadSimple
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m ()
rowLeadSimple pilot av = do
    el "tr" $ do
        el "td" . dynText $ showArea . area <$> av
        el "td" . dynText $ showCoef . coef <$> av
        el "td" . dynText $ showPilot <$> pilot

        return ()

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilotCompare _ sEx xs = do
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-area"))
                        $ text "Area"
                    elAttr "th" ("colspan" =: "3") $ text "Coefficient"
                    el "th" $ text "###-Pilot"
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

                    ePilots :: Dynamic _ [Event _ Pilot] <- el "tbody" $
                            simpleList xs (uncurry (rowLeadCompare mapN) . splitDynPure)
                    let ePilot' :: Event _ Pilot = switchDyn $ leftmost <$> ePilots
                    return ePilot')

            ePilot <- switchHold never ev

            _ <- widgetHold (el "span" $ text "EMPTY") $
                    (\pp -> el "span" $ text (showPilot pp)) <$> ePilot

            return ePilot
    return ()

rowLeadCompare
    :: MonadWidget t m
    => Map.Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m (Event t Pilot)
rowLeadCompare mapN p tl = do
    (pilot, yArea, yAreaDiff, yCoef, yCoefDiff) <- sample . current
                $ ffor2 p tl (\pilot TrackLead{area, coef} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown
                                { leadingArea = area'
                                , leadingCoef = coef'
                                } ->
                            ( pilot
                            , showArea area'
                            , showAreaDiff area' area

                            , showCoef coef'
                            , showCoefDiff coef' coef
                            )

                        _ -> (pilot, "", "", "", ""))

    (eRow, _) <- el' "tr" $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> tl
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        elClass "td" "td-norm td-norm" . text $ yCoef
        elClass "td" "td-norm td-time-diff" . text $ yCoefDiff
        el "td" . dynText $ showPilot <$> p

        return ()

    let ePilot = traceEventWith (T.unpack . showPilot) $ const pilot <$> domEvent Click eRow
    return ePilot
