module FlareTiming.Plot.ArrivalPosition.Table (tableArrivalPosition) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)

tableArrivalPosition
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> m ()
tableArrivalPosition xs xsN = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "table" "table is-striped" $ do
        el "thead" $
            el "tr" $ do
                el "th" $ text "#"
                el "th" $ text "Fraction"
                elClass "th" "th-norm th-norm-arrival" $ text "✓"
                elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                el "th" . dynText $ ffor w hashIdHyphenPilot

                return ()

        tableBody rowArrivalPosition xs xsN

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

rowArrivalPosition :: MonadWidget t m => ShowRow t m
rowArrivalPosition w mapT p ta = do
    (yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\pilot TrackArrival{frac} ->
                    case Map.lookup pilot mapT of
                        Just TrackArrival{frac = fracN} ->
                            ( showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", ""))

    el "tr" $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ ffor2 w p showPilot

        return ()

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="
