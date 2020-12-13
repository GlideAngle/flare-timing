{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.ArrivalPosition.Table (tableArrivalPosition) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Fraction (showArrivalFrac)
import WireTypes.Arrival (TrackArrival(..), ArrivalPlacing(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableArrivalPosition
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackArrival)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableArrivalPosition xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    elClass "table" "table is-striped" $ do
        el "thead" $
            el "tr" $ do
                el "th" $ text "#"
                el "th" $ text "Fraction"
                el "th" . dynText $ ffor w hashIdHyphenPilot

                return ()

        ePilots <- el "tbody" $ simpleList xs (uncurry (rowArrivalPosition w select) . splitDynPure)
        return $ switchDyn $ leftmost <$> ePilots

rowArrivalPosition
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackArrival
    -> m (Event t Pilot)
rowArrivalPosition w select p ta = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showRank . rank <$> ta
        el "td" . dynText $ showArrivalFrac . frac <$> ta
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

showRank :: ArrivalPlacing -> T.Text
showRank (ArrivalPlacing p) = T.pack . show $ p
showRank (ArrivalPlacingEqual p _) = T.pack $ show p ++ "="
