{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.Effort.Table (tableEffort) where

import Reflex.Dom

import WireTypes.Fraction (showEffortFrac)
import WireTypes.Effort (TrackEffort(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (showPilotDistance)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tableEffort
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackEffort)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableEffort xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                elClass "th" "th-plot-effort" $ text "Effort (km)"
                elClass "th" "th-effort-frac" $ text "Fraction"
                el "th" . dynText $ ffor w hashIdHyphenPilot

                return ()

        ePilots <- el "tbody" $ simpleList xs (uncurry (rowEffort w select) . splitDynPure)
        return $ switchDyn $ leftmost <$> ePilots

rowEffort
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackEffort
    -> m (Event t Pilot)
rowEffort w select p te = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-effort" . dynText $ (showPilotDistance 1) . effort <$> te
        el "td" . dynText $ showEffortFrac . frac <$> te
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
