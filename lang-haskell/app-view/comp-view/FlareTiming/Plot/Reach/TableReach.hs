{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.Reach.TableReach (tablePilotReach) where

import Reflex.Dom

import WireTypes.Fraction (showReachFrac)
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (showPilotDistance)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotReach xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                elClass "th" "th-plot-reach" $ text "Reach (km)"
                elClass "th" "th-plot-frac" $ text "Fraction"
                el "th" . dynText $ ffor w hashIdHyphenPilot

                return ()

        ePilots <- el "tbody" $ simpleList xs (uncurry (rowReach w select) . splitDynPure)
        return $ switchDyn $ leftmost <$> ePilots

rowReach
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m (Event t Pilot)
rowReach w select p r = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-reach" . dynText $ (showPilotDistance 1) . reach <$> r
        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> r
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot
        return ()

    let ePilot = const pilot <$> domEvent Click eRow
    return ePilot
