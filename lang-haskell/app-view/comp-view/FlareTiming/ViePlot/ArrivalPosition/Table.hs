{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.ViePlot.ArrivalPosition.Table (tableVieArrivalPosition) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (showArrivalFrac, showArrivalFracDiff)
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..), PilotId(..), fstPilotId, pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableVieArrivalPosition
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableVieArrivalPosition xs xsN select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $
                el "tr" $ do
                    el "th" $ text "#"
                    el "th" $ text "Fraction"
                    elClass "th" "th-norm th-norm-arrival" $ text "✓"
                    elClass "th" "th-norm th-arrival-diff" $ text "Δ"
                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            dyn $ ffor xsN (\xsN' -> do
                    let mapN = Map.fromList $ fstPilotId <$> xsN'
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowArrivalPosition w select mapN) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

    switchHold never ev

rowArrivalPosition
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map PilotId TrackArrival
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m (Event t Pilot)
rowArrivalPosition w select mapT p ta = do
    pilot <- sample $ current p
    (yFrac, yFracDiff) <- sample . current
                $ ffor2 p ta (\(Pilot (pid, _)) TrackArrival{frac} ->
                    case Map.lookup pid mapT of
                        Just TrackArrival{frac = fracN} ->
                            ( showArrivalFrac fracN
                            , showArrivalFracDiff fracN frac
                            )

                        _ -> ("", ""))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="
