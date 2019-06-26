module FlareTiming.Plot.Lead.View (leadPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Lead.Plot as P (leadPlot)

import WireTypes.Fraction (Fractions(..), LeadingFraction(..))
import WireTypes.Comp (Tweak(..), LwScaling(..))
import WireTypes.Lead
    (TrackLead(..), LeadingArea(..), LeadingCoefficient(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (Points(..), LeadingPoints(..))
import FlareTiming.Pilot (showPilotName)

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

leadPlot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
leadPlot tweak sEx ld = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ ld'
                                _ <- P.leadPlot (_element_raw elPlot) (lcRange xs) (placings xs)
                                return ())
                            ]

                        elClass "div" "level" $
                            elClass "div" "level-item" $
                                el "ul" $ do
                                    el "li" $ do
                                        elClass "span" "legend-leading" $ text "─"
                                        text " GAP equation"
                                    el "li" $ do
                                        elClass "span" "legend-leading" $ text "- -"
                                        text " FS equation"

                        ld' <- sample . current $ ld

                    return ()

        elClass "div" "tile is-child" $ tablePilot tweak sEx ld

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilot tweak sEx xs = do
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
                    el "th" $ text "Pilot"

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
        el "td" . dynText $ showFrac . frac <$> av
        el "td" . dynText $ showPilotName <$> pilot

        return ()

tablePilotCompare
    :: MonadWidget t m
    => Dynamic t (Maybe Tweak)
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilotCompare _ sEx xs = do
    let pts = fmap ((\Points{leading = LeadingPoints x} -> x) . Norm.breakdown . snd) <$> sEx
    let maxPts = ffor pts (\case [] -> 0; pts' -> maximum pts')
    let sEx' = Map.fromList <$> sEx
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-area"))
                        $ text "Area"
                    elAttr "th" ("colspan" =: "3") $ text "Coefficient"
                    elAttr "th" ("colspan" =: "3" <> ("class" =: "th-lead-frac"))
                        $ text "Fraction"
                    el "th" $ text "Pilot"
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

            el "tbody" $ do
                simpleList xs (uncurry (rowLeadCompare maxPts sEx') . splitDynPure)

    return ()

rowLeadCompare
    :: MonadWidget t m
    => Dynamic t Double
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m ()
rowLeadCompare _maxPts sEx pilot av = do
    (yArea, yAreaDiff, yCoef, yCoefDiff, yFrac, pFrac) <- sample . current
                $ ffor3 pilot sEx av (\pilot' sEx' TrackLead{area, coef, frac} ->
                    case Map.lookup pilot' sEx' of
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

                            , showFrac frac'
                            , showFracDiff frac' frac
                            )

                        _ -> ("", "", "", "", "", ""))

    el "tr" $ do
        elClass "td" "td-lead-area" . dynText $ showArea . area <$> av
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        elClass "td" "td-lead-coef" . dynText $ showCoef . coef <$> av
        elClass "td" "td-norm td-norm" . text $ yCoef
        elClass "td" "td-norm td-time-diff" . text $ yCoefDiff
        elClass "td" "td-lead-frac" . dynText $ showFrac . frac <$> av
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ pFrac
        el "td" . dynText $ showPilotName <$> pilot

        return ()

showArea :: LeadingArea -> T.Text
showArea (LeadingArea a) = T.pack $ printf "%.0f" a

showCoef :: LeadingCoefficient -> T.Text
showCoef (LeadingCoefficient lc) = T.pack $ printf "%.3f" lc

showFrac :: LeadingFraction -> T.Text
showFrac (LeadingFraction x) = T.pack $ printf "%.3f" x

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

showFracDiff :: LeadingFraction -> LeadingFraction -> T.Text
showFracDiff (LeadingFraction expected) (LeadingFraction actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"


