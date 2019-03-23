module FlareTiming.Plot.Lead.View (hgPlot) where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import qualified FlareTiming.Plot.Lead.Plot as P (hgPlot)

import WireTypes.Lead
    (TrackLead(..), LeadingArea(..), LeadingCoefficient(..), LeadingFraction(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Point (LeadingPoints(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Task.Score.Show

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

hgPlot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
hgPlot sEx ld = do
    pb <- delay 1 =<< getPostBuild

    elClass "div" "tile is-ancestor" $ do
        elClass "div" "tile" $
            elClass "div" "tile is-parent" $
                elClass "div" "tile is-child" $ do
                    (elPlot, _) <- elAttr' "div" (("id" =: "hg-plot-lead") <> ("style" =: "height: 460px;width: 640px")) $ return ()
                    rec performEvent_ $ leftmost
                            [ ffor pb (\_ -> liftIO $ do
                                let xs = snd . unzip $ ld'
                                _ <- P.hgPlot (_element_raw elPlot) (lcRange xs) (placings xs)
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

        elClass "div" "tile is-child" $ tablePilot sEx ld

    return ()

tablePilot
    :: MonadWidget t m
    => Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackLead)]
    -> m ()
tablePilot sEx xs = do
    let pts = fmap ((\(LeadingPoints x) -> x) . Norm.leading . snd) <$> sEx
    let maxPts = ffor pts (\case [] -> 0; pts' -> maximum pts')
    let sEx' = Map.fromList <$> sEx
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Area"
                    elClass "th" "th-norm" $ text "✓-Area"
                    elClass "th" "th-norm" $ text "Δ-Area"
                    el "th" $ text "Coef"
                    elClass "th" "th-norm" $ text "✓-Coef"
                    elClass "th" "th-norm" $ text "Δ-Coef"
                    el "th" $ text "Frac"
                    elClass "th" "th-norm" $ text "✓-Frac"
                    elClass "th" "th-norm" $ text "Δ-Frac"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry (rowLead maxPts sEx') . splitDynPure)

    return ()

rowLead
    :: MonadWidget t m
    => Dynamic t Double
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t Pilot
    -> Dynamic t TrackLead
    -> m ()
rowLead _maxPts sEx pilot av = do
    (yArea, yAreaDiff, yCoef, yCoefDiff, yFrac, pFrac) <- sample . current
                $ ffor3 pilot sEx av (\pilot' sEx' TrackLead{area, coef, frac} ->
                    case Map.lookup pilot' sEx' of
                        Just
                            Norm.NormBreakdown
                                { leadingArea = area'
                                , leadingCoef = coef'
                                , leadingFrac = frac'
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
        el "td" . dynText $ showArea . area <$> av
        elClass "td" "td-norm td-norm" . text $ yArea
        elClass "td" "td-norm td-time-diff" . text $ yAreaDiff
        el "td" . dynText $ showCoef . coef <$> av
        elClass "td" "td-norm td-norm" . text $ yCoef
        elClass "td" "td-norm td-time-diff" . text $ yCoefDiff
        el "td" . dynText $ showFrac . frac <$> av
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
    | actual == expected = "="
    | otherwise = T.pack $ printf "%+.0f" (actual - expected)

showCoefDiff :: LeadingCoefficient -> LeadingCoefficient -> T.Text
showCoefDiff (LeadingCoefficient expected) (LeadingCoefficient actual)
    | actual == expected = "="
    | otherwise = T.pack $ printf "%+.3f" (actual - expected)

showFracDiff :: LeadingFraction -> LeadingFraction -> T.Text
showFracDiff (LeadingFraction expected) (LeadingFraction actual)
    | actual == expected = "="
    | otherwise = T.pack $ printf "%+.3f" (actual - expected)


