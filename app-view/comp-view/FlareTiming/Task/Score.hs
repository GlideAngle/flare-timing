module FlareTiming.Task.Score (tableScore) where

import Prelude hiding (min)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack, breakOn)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Route (TaskLength(..), showTaskDistance)
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
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
import WireTypes.Comp (UtcOffset(..), Discipline(..))
import WireTypes.Pilot (Pilot(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (showHmsForHours, showT, timeZone)

speedSection :: Maybe TaskLength -> T.Text
speedSection =
    T.pack
    . maybe
        "Speed Section"
        (\TaskLength{..} ->
            let tr = showTaskDistance taskRoute
                ss = showTaskDistance taskRouteSpeedSubset
            in printf "Speed Section (%s of %s)" ss tr)

tableScore
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Discipline
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t [Pilot]
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe Wg.Weights)
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t [(Pilot, Breakdown)]
    -> m ()
tableScore utcOffset hgOrPg ln dnf vy wg pt tp xs = do
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenPlaces :: Int <- sample . current $ length <$> xs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenPlaces + 1

    let thSpace = elClass "th" "th-space" $ text ""

    let tableClass =
            let tc = " table is-striped is-narrow is-fullwidth" in
            ffor hgOrPg ((<> tc) . T.pack . show)

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "10") $ text ""
                elAttr "th" ("colspan" =: "7" <> "class" =: "th-points") $ text "Points"

            el "tr" $ do
                elAttr "th" ("rowspan" =: "2" <> "class" =: "th-placing") $ text "#"
                elAttr "th" ("rowspan" =: "2" <> "class" =: "th-pilot") $ text "Pilot"
                elAttr "th" ("colspan" =: "6" <> "class" =: "th-speed-section") . dynText
                    $ speedSection <$> ln
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-distance") $ text "Distance Flown"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points-breakdown") $ text "Points for Distance"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-other-points") $ text ""
                elClass "th" "th-total-points" $ text ""

            el "tr" $ do
                elClass "th" "th-start-start" $ text "Start"
                elClass "th" "th-start-gate" $ text "Gate"
                elClass "th" "th-end" $ text "End"
                elClass "th" "th-time" $ text "Time ‖"
                elClass "th" "th-pace" $ text "Pace ¶"
                elClass "th" "th-speed" $ text "Velocity"

                elClass "th" "th-best-distance" $ text "Reach †"
                elClass "th" "th-landed-distance" $ text "Landed"
                elClass "th" "th-reach-points" $ text "Reach ‡"
                elClass "th" "th-effort-points" $ text "Effort §"

                elClass "th" "th-distance-points" $ text "Subtotal"
                elClass "th" "th-lead-points" $ text "Lead"
                elClass "th" "th-time-points" $ text "Time"
                elClass "th" "th-arrival-points" $ text "Arrival"
                elClass "th" "th-total-points" $ text "Total"

            elClass "tr" "tr-validity" $ do

                elAttr "th" ("colspan" =: "2" <> "class" =: "th-launch-validity") . dynText $
                    maybe
                        ""
                        ( (\v ->
                            "Validity (Launch = "
                            <> showLaunchValidity v
                            <> ")")
                        . Vy.launch
                        )
                    <$> vy

                elAttr "th" ("colspan" =: "8") $ text ""

                thSpace
                thSpace

                elClass "th" "th-distance-validity" . dynText $
                    maybe
                        ""
                        ( showDistanceValidity
                        . Vy.distance
                        )
                    <$> vy

                thSpace

                elClass "th" "th-time-validity" . dynText $
                    maybe
                        ""
                        ( showTimeValidity
                        . Vy.time
                        )
                    <$> vy

                thSpace

                elClass "th" "th-task-validity" . dynText $
                    maybe
                        ""
                        ( showTaskValidity
                        . Vy.task
                        )
                    <$> vy

            elClass "tr" "tr-weight" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-weight") $ text "Weights"
                elAttr "th" ("colspan" =: "8") $ text ""

                thSpace
                thSpace

                elClass "th" "th-distance-weight" . dynText $
                    maybe
                        ""
                        ( showDistanceWeight
                        . Wg.distance
                        )
                    <$> wg

                elClass "th" "th-leading-weight" . dynText$
                    maybe
                        ""
                        ( showLeadingWeight
                        . Wg.leading
                        )
                    <$> wg

                elClass "th" "th-time-weight" . dynText$
                    maybe
                        ""
                        ( showTimeWeight
                        . Wg.time
                        )
                    <$> wg

                elClass "th" "th-arrival-weight" . dynText$
                    maybe
                        ""
                        ( showArrivalWeight
                        . Wg.arrival
                        )
                    <$> wg

                thSpace

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elAttr "th" ("colspan" =: "5") $ text ""
                elClass "th" "th-speed-units" $ text "(km/h)"
                elClass "th" "th-best-distance-units" $ text "(km)"
                elClass "th" "th-landed-distance-units" $ text "(km)"

                elClass "th" "th-reach-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showLinearPoints (Just x) x)
                        . Pt.reach
                        )
                    <$> pt

                elClass "th" "th-effort-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showDifficultyPoints (Just x) x)
                        . Pt.effort
                        )
                    <$> pt

                elClass "th" "th-distance-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showDistancePoints (Just x) x)
                        . Pt.distance
                        )
                    <$> pt

                elClass "th" "th-leading-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showLeadingPoints (Just x) x)
                        . Pt.leading
                        )
                    <$> pt

                elClass "th" "th-time-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTimePoints (Just x) x)
                        . Pt.time
                        )
                    <$> pt

                elClass "th" "th-arrival-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showArrivalPoints (Just x) x)
                        . Pt.arrival
                        )
                    <$> pt

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPoints (Just x) x)
                    <$> tp

        _ <- el "tbody" $ do
            _ <- simpleList xs (pointRow utcOffset pt tp)
            dnfRows dnfPlacing dnf
            return ()

        el "tfoot" $ do
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "* Any points so annotated are the maximum attainable."
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "† How far along the course, reaching goal or elsewhere. The distance reached in the air can be further than the distance at landing."
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "‡ Points award for reach are also called linear distance points."
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "§ Points award for effort are also called distance difficulty points."
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "‖ \"Time\" is the time across the speed section from time zero of the start gate taken."
            el "tr" $
                elAttr "td" ("colspan" =: "17")
                    $ text "¶ \"Pace\" is the time across the speed section from the time of crossing the start for the last time."
    return ()

pointRow
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow utcOffset pt tp x = do
    let tz = timeZone <$> utcOffset
    let pilot = fst <$> x
    let b = snd <$> x
    let points = breakdown . snd <$> x
    let v = velocity . snd <$> x

    el "tr" $ do
        elClass "td" "td-placing" . dynText $ showRank . place <$> b
        elClass "td" "td-pilot" . dynText $ showPilotName <$> pilot
        elClass "td" "td-start-start" . dynText $ zipDynWith showSs tz v
        elClass "td" "td-start-gate" . dynText $ zipDynWith showGs tz v
        elClass "td" "td-end" . dynText $ zipDynWith showEs tz v
        elClass "td" "td-time" . dynText $ showGsVelocityTime <$> v
        elClass "td" "td-pace" . dynText $ showSsVelocityTime <$> v
        elClass "td" "td-speed" . dynText $ showVelocityVelocity <$> v

        elClass "td" "td-best-distance" . dynText
            $ maybe "" showPilotDistance . reachDistance <$> b
        elClass "td" "td-landed-distance" . dynText
            $ maybe "" showPilotDistance . landedDistance <$> b

        elClass "td" "td-reach-points" . dynText $ showMax Pt.reach showLinearPoints pt points
        elClass "td" "td-effort-points" . dynText $ showMax Pt.effort showDifficultyPoints pt points
        elClass "td" "td-distance-points" . dynText $ showMax Pt.distance showDistancePoints pt points
        elClass "td" "td-leading-points" . dynText $ showMax Pt.leading showLeadingPoints pt points
        elClass "td" "td-time-points" . dynText $ showMax Pt.time showTimePoints pt points
        elClass "td" "td-arrival-points" . dynText $ showMax Pt.arrival showArrivalPoints pt points

        elClass "td" "td-total-points" . dynText $ zipDynWith showTaskPoints tp (total <$> b)

dnfRows
    :: MonadWidget t m
    => TaskPlacing
    -> Dynamic t [Pilot]
    -> m ()
dnfRows place ps = do
    len <- sample . current $ length <$> ps
    let p1 = take 1 <$> ps
    let pN = drop 1 <$> ps

    case len of
        0 -> do
            return ()
        1 -> do
            _ <- simpleList ps (dnfRow place (Just 1))
            return ()
        n -> do
            _ <- simpleList p1 (dnfRow place (Just n))
            _ <- simpleList pN (dnfRow place Nothing)
            return ()

dnfRow
    :: MonadWidget t m
    => TaskPlacing
    -> Maybe Int
    -> Dynamic t Pilot
    -> m ()
dnfRow place rows pilot = do
    let dnfMega =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: "14"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ showPilotName <$> pilot
        dnfMega
        elClass "td" "td-total-points" $ text "0"
        return ()

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

showGs :: TimeZone -> Velocity -> T.Text
showGs tz Velocity{gs = Just (Pt.StartGate t)} = showT tz t
showGs _ _ = ""

showEs :: TimeZone -> Velocity -> T.Text
showEs tz Velocity{es = Just t} = showT tz t
showEs _ _ = ""

showSsVelocityTime :: Velocity -> T.Text
showSsVelocityTime Velocity{ssElapsed = Just (PilotTime t)} =
    showHmsForHours . T.pack $ t

showSsVelocityTime _ = ""

showGsVelocityTime :: Velocity -> T.Text
showGsVelocityTime Velocity{gsElapsed = Just (PilotTime t)} =
    showHmsForHours . T.pack $ t

showGsVelocityTime _ = ""

showVelocityVelocity :: Velocity -> T.Text
showVelocityVelocity Velocity{gsVelocity = Just (PilotVelocity v)} =
    fst . T.breakOn " km / h" . T.pack $ v
showVelocityVelocity _ = ""
