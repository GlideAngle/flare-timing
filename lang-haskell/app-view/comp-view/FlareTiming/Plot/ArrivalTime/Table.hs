module FlareTiming.Plot.ArrivalTime.Table (tableArrivalTime) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival
    ( TrackArrival(..), ArrivalPlacing(..), ArrivalLag(..)
    , showArrivalLagDiff)
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot)
import FlareTiming.Time (showHmsForHours, showHours)

tableArrivalTime
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tableArrivalTime xs xsN =
    elClass "table" "table is-striped" $ do
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

        tableBody rowArrivalTime xs xsN

        return ()

type ShowRow t m
    = Dynamic t Int
    -> Map.Map Pilot TrackArrival
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m ()

tableBody
    :: MonadWidget t m
    => ShowRow t m
    -> Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tableBody showRow xs xsN = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    el "tbody" $ do
        _ <- dyn $ ffor xsN (\xsN' -> do
                let mapT = Map.fromList xsN'

                simpleList xs (uncurry (showRow w mapT) . splitDynPure))

        return ()
    return ()

rowArrivalTime :: MonadWidget t m => ShowRow t m
rowArrivalTime w mapT p ta = do
    (yLag, yLagDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\pilot TrackArrival{frac, lag} ->
                    case Map.lookup pilot mapT of
                        Just TrackArrival{frac = fracN, lag = lagN} ->
                            ( showHr lagN
                            , showArrivalLagDiff lagN lag
                            , showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", "", "", ""))

    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showHms . lag <$> ta
        el "td" . dynText $ showHr . lag <$> ta
        elClass "td" "td-norm" . text $ yLag
        elClass "td" "td-norm" . text $ yLagDiff
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ ffor2 w p showPilot

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="

showHr :: ArrivalLag -> T.Text
showHr (ArrivalLag x) = showHours x

showHms :: ArrivalLag -> T.Text
showHms (ArrivalLag x) = showHmsForHours x
