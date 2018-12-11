module FlareTiming.Task.Score (tableScore) where

import Prelude hiding (min)
import Reflex.Dom
import qualified Data.Text as T (Text, pack, breakOn)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone, utcToLocalTime)

import qualified WireTypes.Point as Pt (Points(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.Point
    ( TaskPlacing(..)
    , TaskPoints(..)
    , Breakdown(..)
    , Velocity(..)
    , PilotTime(..)
    , PilotVelocity(..)

    , showPilotDistance

    , showLinearPoints
    , showDifficultyPoints
    , showDistancePoints
    , showArrivalPoints
    , showLeadingPoints
    , showTimePoints
    , showTaskPoints

    , showDistanceWeight
    , showArrivalWeight
    , showLeadingWeight
    , showTimeWeight
    )
import WireTypes.Validity
    ( showLaunchValidity
    , showDistanceValidity
    , showTimeValidity
    , showTaskValidity
    )
import WireTypes.Comp (UtcOffset(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (showHmsForHours)

tableScore
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe Wg.Weights)
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t [(Pilot, Breakdown)]
    -> m ()
tableScore utcOffset vy wg pt tp xs = do
    let classR = "class" =: "has-text-right"
    let classBg = "class" =: "has-text-centered has-background-white-bis"
    let classC = "class" =: "has-text-centered"
    let thR = elClass "th" "has-text-right" . text
    let thBg = elClass "th" "has-text-right has-background-white-bis" . text
    let thStart = elClass "th" "has-text-centered has-text-success" . text
    let thEnd = elClass "th" "has-text-centered has-text-danger" . text
    let thC = elClass "th" "has-text-centered" . text
    let thU = elClass "th" "has-text-right" . text
    let thPt = elClass "th" "has-text-right has-background-white" . dynText
    let thVy = elClass "th" "has-text-right has-background-white" . dynText
    let thSpace = elClass "th" "has-text-right has-background-white" $ text ""

    _ <- elClass "table" "table is-narrow is-fullwidth" $
            el "thead" $ do

                el "tr" $ do
                    elAttr "th" ("rowspan" =: "2" <> classR) $ text "#"
                    elAttr "th" ("rowspan" =: "2") $ text "Pilot"
                    elAttr "th" ("colspan" =: "4" <> classC) $ text "Speed Section"
                    elAttr "th" classC $ text "Best"
                    elAttr "th" ("colspan" =: "3" <> classBg) $ text "Distance Point Breakdown"
                    elAttr "th" ("colspan" =: "4" <> classBg) $ text "Other Points"

                el "tr" $ do
                    thStart "Start"
                    thEnd "End"
                    thC "Time"
                    thR "Speed"
                    thC "Distance"
                    thBg "Reach"
                    thBg "Effort"
                    thBg "Distance"
                    thBg "Lead"
                    thBg "Time"
                    thBg "Arrival"
                    thBg "Total"

                elClass "tr" "is-italic has-background-white-bis" $ do
                    el "th" $ text ""

                    elAttr "th" classR . dynText $
                        maybe
                            ""
                            ( (\v ->
                                "Validity (Launch = "
                                <> showLaunchValidity v
                                <> ")")
                            . Vy.launch
                            )
                        <$> vy

                    elAttr "th" ("colspan" =: "5") $ text ""

                    thSpace
                    thSpace

                    thVy $
                        maybe
                            ""
                            ( showDistanceValidity
                            . Vy.distance
                            )
                        <$> vy

                    thSpace

                    thVy $
                        maybe
                            ""
                            ( showTimeValidity
                            . Vy.time
                            )
                        <$> vy

                    thSpace

                    thVy $
                        maybe
                            ""
                            ( showTaskValidity
                            . Vy.task
                            )
                        <$> vy

                elClass "tr" "is-italic has-background-white-bis" $ do
                    elAttr "th" ("colspan" =: "2" <> classR) $ text "Weights"
                    elAttr "th" ("colspan" =: "5") $ text ""

                    thSpace
                    thSpace

                    thPt $
                        maybe
                            ""
                            ( showDistanceWeight
                            . Wg.distance
                            )
                        <$> wg

                    thPt $
                        maybe
                            ""
                            ( showLeadingWeight
                            . Wg.leading
                            )
                        <$> wg

                    thPt $
                        maybe
                            ""
                            ( showTimeWeight
                            . Wg.time
                            )
                        <$> wg

                    thPt $
                        maybe
                            ""
                            ( showArrivalWeight
                            . Wg.arrival
                            )
                        <$> wg

                    thSpace

                elClass "tr" "is-italic has-background-white-bis" $ do
                    elAttr "th" ("colspan" =: "2" <> classR) $ text "Available Points (Units)"
                    elAttr "th" ("colspan" =: "3") $ text ""
                    thU "(km/h)"
                    thU "(km)"

                    thPt $
                        maybe
                            ""
                            ( (\x -> showLinearPoints (Just x) x)
                            . Pt.reach
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            ( (\x -> showDifficultyPoints (Just x) x)
                            . Pt.effort
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            ( (\x -> showDistancePoints (Just x) x)
                            . Pt.distance
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            ( (\x -> showLeadingPoints (Just x) x)
                            . Pt.leading
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            ( (\x -> showTimePoints (Just x) x)
                            . Pt.time
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            ( (\x -> showArrivalPoints (Just x) x)
                            . Pt.arrival
                            )
                        <$> pt

                    thPt $
                        maybe
                            ""
                            (\x -> showTaskPoints (Just x) x)
                        <$> tp

                simpleList xs (row utcOffset pt tp)

    return ()

row
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
row utcOffset pt tp x = do
    let tz = timeZone <$> utcOffset
    let pilot = fst <$> x
    let b = snd <$> x
    let points = breakdown . snd <$> x
    let v = velocity . snd <$> x

    let tdR = elClass "td" "has-text-right" . dynText
    let tdR' = elClass "td" "has-text-right has-background-white-bis" . dynText
    let tdStart =
            elClass
                "td"
                "has-text-centered has-text-success"
            . dynText

    let tdEnd =
            elClass
                "td"
                "has-text-centered has-text-danger"
            . dynText

    let tdPlace =
            elClass
                "td"
                "has-text-right has-text-weight-bold"
            . dynText

    let tdTotal =
            elClass
                "td"
                "has-text-right has-text-weight-bold has-background-white-bis"
            . dynText

    el "tr" $ do
        tdPlace $ showRank . place <$> b
        el "td" . dynText $ showPilotName <$> pilot
        tdStart $ zipDynWith showSs tz v
        tdEnd $ zipDynWith showEs tz v
        tdR $ showVelocityTime <$> v
        tdR $ showVelocityVelocity <$> v
        tdR $ showVelocityDistance <$> v

        tdR' $ showMax Pt.reach showLinearPoints pt points
        tdR' $ showMax Pt.effort showDifficultyPoints pt points
        tdR' $ showMax Pt.distance showDistancePoints pt points
        tdR' $ showMax Pt.leading showLeadingPoints pt points
        tdR' $ showMax Pt.time showTimePoints pt points
        tdR' $ showMax Pt.arrival showArrivalPoints pt points

        tdTotal $
            zipDynWith
                showTaskPoints
                tp
                (total <$> b)


showMax
    :: (Reflex t, Functor f)
    => (a -> b)
    -> (f b -> b -> c)
    -> Dynamic t (f a)
    -> Dynamic t a
    -> Dynamic t c
showMax getField f pt points =
    zipDynWith
        f
        ((fmap . fmap) getField pt)
        (getField <$> points)

showRank :: TaskPlacing -> T.Text
showRank (TaskPlacing p) = T.pack . show $ p
showRank (TaskPlacingEqual p) = T.pack $ show p ++ "="

showSs :: TimeZone -> Velocity -> T.Text
showSs tz Velocity{ss = Just t} = showT tz t
showSs _ _ = ""

showEs :: TimeZone -> Velocity -> T.Text
showEs tz Velocity{es = Just t} = showT tz t
showEs _ _ = ""

showVelocityTime :: Velocity -> T.Text
showVelocityTime Velocity{gsElapsed = Just (PilotTime t)} =
    showHmsForHours . T.pack $ t

showVelocityTime _ = ""

showVelocityVelocity :: Velocity -> T.Text
showVelocityVelocity Velocity{gsVelocity = Just (PilotVelocity v)} =
    fst . T.breakOn " km / h" . T.pack $ v
showVelocityVelocity _ = ""

showVelocityDistance :: Velocity -> T.Text
showVelocityDistance Velocity{distance = Just d} = showPilotDistance d
showVelocityDistance _ = ""

showT :: TimeZone -> UTCTime -> T.Text
showT tz = 
    T.pack
    . formatTime defaultTimeLocale "%T"
    . utcToLocalTime tz

timeZone :: UtcOffset -> TimeZone
timeZone UtcOffset{timeZoneMinutes = tzMins} = minutesToTimeZone tzMins
