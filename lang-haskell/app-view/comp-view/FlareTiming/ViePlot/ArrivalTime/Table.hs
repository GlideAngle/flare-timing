{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.ArrivalTime.Table (tableVieArrivalTime) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival
    ( TrackArrival(..), ArrivalPlacing(..), ArrivalLag(..)
    , showArrivalLagDiff)
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot)
import FlareTiming.Time (showHmsForHours, showHours)
import FlareTiming.Plot.Event (rowClass)

tableVieArrivalTime
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableVieArrivalTime xs xsN select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text " "
                    elAttr "th" ("colspan" =: "2") $ text "Lag"
                    elAttr "th" ("colspan" =: "6") $ text ""

                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "HH:MM:SS"
                    el "th" $ text "H.hhh"
                    elClass "th" "th-norm th-norm-arrival" $ text "✓"
                    elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                    el "th" $ text "Fraction"
                    elClass "th" "th-norm th-norm-arrival" $ text "✓"
                    elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                    el "th" $ text ""

                    return ()

            dyn $ ffor xsN (\xsN' -> do
                    let mapN = Map.fromList xsN'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowArrivalTime w select mapN) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

    switchHold never ev

rowArrivalTime
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map Pilot TrackArrival
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m (Event t Pilot)
rowArrivalTime w select mapT p ta = do
    pilot <- sample $ current p

    (yLag, yLagDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\p' TrackArrival{frac, lag} ->
                    case Map.lookup p' mapT of
                        Just TrackArrival{frac = fracN, lag = lagN} ->
                            ( showHr lagN
                            , showArrivalLagDiff lagN lag
                            , showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", "", "", ""))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showHms . lag <$> ta
        el "td" . dynText $ showHr . lag <$> ta
        elClass "td" "td-norm" . text $ yLag
        elClass "td" "td-norm" . text $ yLagDiff
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

    return $ const pilot <$> domEvent Click eRow

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="

showHr :: ArrivalLag -> T.Text
showHr (ArrivalLag x) = showHours x

showHms :: ArrivalLag -> T.Text
showHms (ArrivalLag x) = showHmsForHours x
