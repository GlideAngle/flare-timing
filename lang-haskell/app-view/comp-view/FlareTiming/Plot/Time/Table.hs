{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.Time.Table (tableSpeed) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Fraction (showSpeedFrac)
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..))
import qualified WireTypes.Speed as Speed (TrackSpeed(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (StartGate)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Time (showHmsForHours, showHours)
import FlareTiming.Plot.Event (rowClass)

tableSpeed
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tableSpeed _sgs _sEx xs select = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    elClass "table" "table is-striped" $ do
            el "thead" $ do

                el "tr" $ do
                    el "th" $ text "H.hhh"
                    el "th" $ text "HH:MM:SS"
                    el "th" $ text "Fraction"
                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            ePilots <- el "tbody" $ simpleList xs (uncurry (rowSpeed w select) . splitDynPure)
            return $ switchDyn $ leftmost <$> ePilots

rowSpeed
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Dynamic t Pilot
    -> Dynamic t TrackSpeed
    -> m (Event t Pilot)
rowSpeed w select p ts = do
    pilot <- sample $ current p

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        el "td" . dynText $ showHr . Speed.time <$> ts
        el "td" . dynText $ showHms . Speed.time <$> ts
        el "td" . dynText $ showSpeedFrac . frac <$> ts
        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow

showHr :: PilotTime -> T.Text
showHr (PilotTime x) = showHours x

showHms :: PilotTime -> T.Text
showHms (PilotTime x) = showHmsForHours x
