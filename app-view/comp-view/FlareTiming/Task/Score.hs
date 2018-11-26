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
                    el "th" $ text "Id"
                    el "th" $ text "Pilot"
                    el "th" $ text "Total"
                simpleList xs row

    return ()

row
    :: MonadWidget t m
    => Dynamic t (Pilot, Breakdown)
    -> m ()
row x = do
    let p = fst <$> x
    let b = snd <$> x

    let td = el "td" . dynText
    let tdR = elClass "td" "has-text-right" . dynText

    el "tr" $ do
        tdR $ showPilotId <$> p
        td $ showPilotName <$> p
        tdR $ showTotal . total <$> b

showTotal :: TaskPoints -> T.Text
showTotal (TaskPoints p) = T.pack . show $ truncate p
