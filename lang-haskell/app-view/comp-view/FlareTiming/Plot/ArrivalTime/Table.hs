{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.ArrivalTime.Table (tableArrivalTime) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Fraction (showArrivalFrac)
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..), ArrivalLag(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot)
import FlareTiming.Time (showHmsForHours, showHours)
import FlareTiming.Plot.Event (rowClass)

tableArrivalTime
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableArrivalTime xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text " "
                elAttr "th" ("colspan" =: "2") $ text "Lag"
                elAttr "th" ("colspan" =: "2") $ text ""

            el "tr" $ do
                el "th" $ text ""
                el "th" $ text "HH:MM:SS"
                el "th" $ text "H.hhh"
                el "th" $ text "Fraction"
                el "th" $ text ""

                return ()

            ePilots <- el "tbody" $ simpleList xs (uncurry (rowArrivalTime w select) . splitDynPure)
            return $ switchDyn $ leftmost <$> ePilots

rowArrivalTime
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m (Event t Pilot)
rowArrivalTime w select p ta = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showHms . lag <$> ta
        el "td" . dynText $ showHr . lag <$> ta
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

    return $ const pilot <$> domEvent Click eRow

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="

showHr :: ArrivalLag -> T.Text
showHr (ArrivalLag x) = showHours x

showHms :: ArrivalLag -> T.Text
showHms (ArrivalLag x) = showHmsForHours x
