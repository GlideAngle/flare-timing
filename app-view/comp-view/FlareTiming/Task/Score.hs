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
    , PilotVelocity(..)
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
                    elAttr "th" ("class" =: "has-text-right" <> "rowspan" =: "2") $ text "Id"
                    elAttr "th" ("rowspan" =: "2") $ text "Pilot"
                    elAttr "th" ("colspan" =: "5" ) $ text "Velocity"
                    elAttr "th" ("colspan" =: "5") $ text "Points"
                el "tr" $ do
                    el "th" $ text "SS"
                    el "th" $ text "ES"
                    el "th" $ text "Time"
                    el "th" $ text "Velocity"
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
        tdR $ showSs <$> v
        tdR $ showEs <$> v
        tdR $ showVelocityTime <$> v
        tdR $ showVelocityVelocity <$> v
        tdR $ showVelocityDistance <$> v
        tdR $ showDistancePoints . (\Points{distance = d} -> d) <$> points
        tdR $ showLeadingPoints . leading <$> points
        tdR $ showTimePoints . time <$> points
        tdR $ showArrivalPoints . arrival <$> points
        tdR $ showTotal . total <$> b

showTotal :: TaskPoints -> T.Text
showTotal (TaskPoints p) = T.pack . show $ (truncate p :: Integer)

showSs :: Velocity -> T.Text
showSs Velocity{ss = Just t} = T.pack . show $ t
showSs _ = ""

showEs :: Velocity -> T.Text
showEs Velocity{es = Just t} = T.pack . show $ t
showEs _ = ""

showVelocityTime :: Velocity -> T.Text
showVelocityTime Velocity{gsElapsed = Just (PilotTime t)} = T.pack t
showVelocityTime _ = ""

showVelocityVelocity :: Velocity -> T.Text
showVelocityVelocity Velocity{gsVelocity = Just (PilotVelocity v)} = T.pack v
showVelocityVelocity _ = ""

showVelocityDistance :: Velocity -> T.Text
showVelocityDistance Velocity{distance = Just (PilotDistance d)} = T.pack d
showVelocityDistance _ = ""
