module FlareTiming.Task.Score (tableScore) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( Points(..)
    , TaskPoints(..)
    , Breakdown(..)
    , Velocity(..)
    , PilotDistance(..)
    , PilotTime(..)
    , showDistancePoints
    , showArrivalPoints
    , showLeadingPoints
    , showTimePoints
    )
import WireTypes.Pilot (Pilot(..))
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
                    el "th" $ text "Time"
                    el "th" $ text "Distance"
                    el "th" $ text "Distance"
                    el "th" $ text "Lead"
                    el "th" $ text "Time"
                    el "th" $ text "Arrival"
                    el "th" $ text "Total"
                simpleList xs row

    return ()

row
    :: MonadWidget t m
    => Dynamic t (Pilot, Breakdown)
    -> m ()
row x = do
    let pilot = fst <$> x
    let b = snd <$> x
    let points = breakdown . snd <$> x
    let v = velocity . snd <$> x

    let td = el "td" . dynText
    let tdR = elClass "td" "has-text-right" . dynText

    el "tr" $ do
        tdR $ showPilotId <$> pilot
        td $ showPilotName <$> pilot
        tdR $ showVelocityTime <$> v
        tdR $ showVelocityDistance <$> v
        tdR $ showDistancePoints . (\Points{distance = d} -> d) <$> points
        tdR $ showLeadingPoints . leading <$> points
        tdR $ showTimePoints . time <$> points
        tdR $ showArrivalPoints . arrival <$> points
        tdR $ showTotal . total <$> b

showTotal :: TaskPoints -> T.Text
showTotal (TaskPoints p) = T.pack . show $ (truncate p :: Integer)

showVelocityTime :: Velocity -> T.Text
showVelocityTime Velocity{gsElapsed = Just (PilotTime t)} = T.pack t
showVelocityTime _ = ""

showVelocityDistance :: Velocity -> T.Text
showVelocityDistance Velocity{distance = Just (PilotDistance d)} = T.pack d
showVelocityDistance _ = ""
