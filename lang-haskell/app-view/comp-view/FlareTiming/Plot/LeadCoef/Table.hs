module FlareTiming.Plot.LeadCoef.Table (tablePilotCoef) where

import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (Fractions(..), showLeadingFrac, showLeadingFracDiff)
import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead
    (TrackLead(..), LeadingArea(..), LeadingCoefficient(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilot)

tablePilotCoef
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilotCoef tweak sEx xs = do
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
                    el "th" $ text "Frac"
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
        el "td" . dynText $ showLeadingFrac . frac <$> av
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
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-frac"))
                        $ text "Fraction"
                    el "th" $ text "###-Pilot"
                el "tr" $ do
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" $ text ""

                    return ()

            _ <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList xs (uncurry (rowLeadCompare mapN) . splitDynPure))

            return ()
    return ()

rowLeadCompare
    :: MonadWidget t m
    => Map.Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m ()
rowLeadCompare mapN p tl = do
    (yArea, yAreaDiff, yCoef, yCoefDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p tl (\pilot TrackLead{area, coef, frac} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown
                                { leadingArea = area'
                                , leadingCoef = coef'
                                , fractions = Fractions{leading = frac'}
                                } ->
                            ( showArea area'
                            , showAreaDiff area' area

                            , showCoef coef'
                            , showCoefDiff coef' coef

                            , showLeadingFrac frac'
                            , showLeadingFracDiff frac' frac
                            )

                        _ -> ("", "", "", "", "", ""))

    el "tr" $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> tl
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> tl
        elClass "td" "td-norm td-norm" . text $ yCoef
        elClass "td" "td-norm td-time-diff" . text $ yCoefDiff
        elClass "td" "td-lead-frac" . dynText $ showLeadingFrac . frac <$> tl
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ showPilot <$> p

        return ()

showArea :: LeadingArea -> T.Text
showArea (LeadingArea a) = T.pack $ printf "%.0f" a

showCoef :: LeadingCoefficient -> T.Text
showCoef (LeadingCoefficient lc) = T.pack $ printf "%.3f" lc

showAreaDiff :: LeadingArea -> LeadingArea -> T.Text
showAreaDiff (LeadingArea expected) (LeadingArea actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.0f"

showCoefDiff :: LeadingCoefficient -> LeadingCoefficient -> T.Text
showCoefDiff (LeadingCoefficient expected) (LeadingCoefficient actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"
