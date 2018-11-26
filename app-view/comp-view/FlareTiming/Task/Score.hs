module FlareTiming.Task.Score (tableScore) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
    , Breakdown(..)
    )
import WireTypes.Pilot (Pilot(..), PilotName(..))
import FlareTiming.Pilot (showPilotId, showPilotName)

tableScore
    :: MonadWidget t m
    => Dynamic t [(Pilot, Breakdown)]
    -> m ()
tableScore xs = do

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "Points"
                simpleList ((fmap . fmap) fst xs) row

    return ()

row
    :: MonadWidget t m
    => Dynamic t Pilot
    -> m ()
row x = do
    let td = el "td" . dynText
    el "tr" $ do
        td $ showPilotId <$> x
        td $ showPilotName <$> x
