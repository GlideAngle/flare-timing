module FlareTiming.VieScoreFs.Over (tableVieScoreFsOver) where

import Prelude hiding (min)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.Point
    ( TaskPlacing(..)
    , TaskPoints(..)
    , Breakdown(..)
    , PilotDistance(..)
    , ReachToggle(..)
    , showPilotDistance
    , showTaskDistancePoints
    , showTaskArrivalPoints
    , showTaskLeadingPoints
    , showTaskTimePoints
    , showTaskPointsRounded
    , showTaskPointsDiff
    , showTaskPointsDiffStats
    , showRounded
    , showJumpedTheGunTime
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp
    ( UtcOffset(..), Discipline(..), MinimumDistance(..)
    , EarlyStart(..), JumpTheGunLimit(..)
    )
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot, classOfEarlyStart)
import FlareTiming.Time (timeZone)
import FlareTiming.Score.Show

tableVieScoreFsOver
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Discipline
    -> Dynamic t EarlyStart
    -> Dynamic t MinimumDistance
    -> Dynamic t [Pt.StartGate]
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t Dnf
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Wg.Weights)
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t [(Pilot, Breakdown)]
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> m ()
tableVieScoreFsOver utcOffset hgOrPg early free sgs ln dnf' dfNt _vy vw _wg pt tp sDfs sAltFs sAltAs = do
    let w = ffor sDfs (pilotIdsWidth . fmap fst)
    let dnf = unDnf <$> dnf'
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenDfs :: Int <- sample . current $ length <$> sDfs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenDfs + 1

    let thSpace = elClass "th" "th-space" $ text ""

    let tableClass =
            let tc = "table is-striped is-narrow is-fullwidth" in
            ffor2 hgOrPg sgs (\x gs ->
                let y = T.pack . show $ x in
                y <> (if null gs then " " else " sg ") <> tc)

    let cTimePoints =
            let thc = "th-time-points"
                tdc = "td-time-points"
            in
                ffor2 hgOrPg vw (\x vw' ->
                    maybe
                        (thc, tdc)
                        (\ValidityWorking{time = TimeValidityWorking{..}} ->
                            case (x, gsBestTime) of
                                (HangGliding, Nothing) ->
                                    ( "gr-zero " <> thc
                                    , "gr-zero " <> tdc
                                    )
                                (HangGliding, Just _) -> (thc, tdc)
                                (Paragliding, Nothing) ->
                                    ( "gr-zero " <> thc
                                    , "gr-zero " <> tdc
                                    )
                                (Paragliding, Just _) -> (thc, tdc))
                        vw')

    let cArrivalPoints =
            let thc = "th-arrival-points"
                tdc = "td-arrival-points"
            in
                ffor2 hgOrPg vw (\x vw' ->
                    maybe
                        (thc, tdc)
                        (\ValidityWorking{time = TimeValidityWorking{..}} ->
                            case (x, gsBestTime) of
                                (HangGliding, Nothing) ->
                                    ( "gr-zero " <> thc
                                    , "gr-zero " <> tdc
                                    )
                                (HangGliding, Just _) -> (thc, tdc)
                                (Paragliding, _) -> (thc, tdc))
                        vw')

    let yDiff = ffor3 sDfs sAltFs sAltAs (\sDfs' sAltFs' sAltAs' ->
                    let mapFs = Map.fromList sAltFs'
                        mapAs = Map.fromList sAltAs'
                        altTotal Alt.AltBreakdown{total = p} = p
                    in
                        [
                            ( altTotal <$> Map.lookup pilot mapFs
                            , altTotal <$> Map.lookup pilot mapAs
                            , p'
                            )
                        | (pilot, Breakdown{total = p'}) <- sDfs'
                        ])

    let pointStats = ffor
                        yDiff
                        ((\(fs', as', p) ->
                            let fs = sequence fs'
                                as = sequence as'

                                dFs = showTaskPointsDiffStats fs p
                                dAs = showTaskPointsDiffStats as p
                            in
                                T.pack $ printf "Points (FS %s, AS %s)" dFs dAs)
                        . unzip3)

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "4") $ text ""
                elAttr "th" ("colspan" =: "6" <> "class" =: "th-speed-section") . dynText
                    $ showSpeedSection <$> ln
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-distance") $ text "Distance Flown"
                elAttr "th" ("colspan" =: "9" <> "class" =: "th-points") $ dynText pointStats

            el "tr" $ do
                elClass "th" "th-norm th-placing" $ text "✓"
                elClass "th" "th-norm th-placing" $ text "✓"
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-start-early" $ text "Early ¶"
                elClass "th" "th-start-start" $ text "Start"
                elClass "th" "th-start-gate" $ text "Gate"
                elClass "th" "th-time-end" $ text "End"
                elClass "th" "th-time" $ text "Time ‖"
                elClass "th" "th-speed" $ text "Speed"

                elClass "th" "th-min-distance" $ text "Min"
                elClass "th" "th-best-distance" $ text "Reach †"

                elClass "th" "th-distance-points" $ text "Distance"
                elDynClass "th" (fst <$> cTimePoints) $ text "Time"
                elClass "th" "th-leading-points" $ text "Lead"
                elDynClass "th" (fst <$> cArrivalPoints) $ text "Arrival"
                elClass "th" "th-total-points" $ text "Total"
                elClass "th" "th-norm th-total-points" $ text "✓"
                elClass "th" "th-norm th-total-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"
                elClass "th" "th-norm th-diff" $ text "Δ"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elAttr "th" ("colspan" =: "6") $ text ""
                elClass "th" "th-speed-units" $ text "(km/h)"
                elClass "th" "th-min-distance-units" $ text "(km)"
                elClass "th" "th-best-distance-units" $ text "(km)"

                elClass "th" "th-distance-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDistancePoints (Just x) x)
                        . Pt.distance
                        )
                    <$> pt

                elClass "th" "th-time-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskTimePoints (Just x) x)
                        . Pt.time
                        )
                    <$> pt

                elClass "th" "th-leading-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLeadingPoints (Just x) x)
                        . Pt.leading
                        )
                    <$> pt

                elClass "th" "th-arrival-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskArrivalPoints (Just x) x)
                        . Pt.arrival
                        )
                    <$> pt

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPointsRounded (Just x) x)
                    <$> tp

                thSpace
                thSpace
                thSpace
                thSpace

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        w
                        (earliest <$> early)
                        (snd <$> cTimePoints)
                        (snd <$> cArrivalPoints)
                        utcOffset
                        free
                        dfNt
                        pt
                        tp
                        (Map.fromList <$> sAltFs)
                        (Map.fromList <$> sAltAs))

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "21")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† How far along the course, reaching goal or elsewhere. The distance reached in the air can be further than the distance at landing."
            foot "‖ \"Time\" is the time across the speed section from time zero of the start gate taken."
            foot "¶ \"Early\" how much earlier than the start did this pilot jump the gun?"
            foot "☞ Pilots without a tracklog but given a distance by the scorer."
            foot "✓ An expected value as calculated by the official scoring program, FS."
            foot "Δ A difference between a value and an expected value."
            dyn_ $ ffor hgOrPg (\case
                HangGliding -> return ()
                Paragliding -> do
                    el "tr" . tdFoot $ do
                            elClass "span" "pg not" $ text "Arrival"
                            text " points are not scored for paragliding."
                    el "tr" . tdFoot $ do
                            elClass "span" "pg not" $ text "Effort"
                            text " or distance difficulty is not scored for paragliding.")
            dyn_ $ ffor sgs (\gs ->
                if null gs then do
                    el "tr" . tdFoot $ do
                            text "With no "
                            elClass "span" "sg not" $ text "gate"
                            text " to start the speed section "
                            elClass "span" "sg not" $ text "time"
                            text ", the pace clock starts ticking whenever the pilot starts."
                else return ())
            dyn_ $ ffor hgOrPg (\case
                HangGliding ->
                    dyn_ $ ffor vw (\vw' ->
                        maybe
                            (return ())
                            (\ValidityWorking{time = TimeValidityWorking{..}} ->
                                case gsBestTime of
                                    Just _ -> return ()
                                    Nothing -> el "tr" . tdFoot $ do
                                        text "No one made it through the speed section to get "
                                        elClass "span" "gr-zero" $ text "time"
                                        text " and "
                                        elClass "span" "gr-zero" $ text "arrival"
                                        text " points.")
                            vw'
                        )
                Paragliding ->
                    dyn_ $ ffor vw (\vw' ->
                        maybe
                            (return ())
                            (\ValidityWorking{time = TimeValidityWorking{..}} ->
                                case gsBestTime of
                                    Just _ -> return ()
                                    Nothing -> el "tr" . tdFoot $ do
                                        text "No one made it through the speed section to get "
                                        elClass "span" "gr-zero" $ text "time"
                                        text " points.")
                            vw'
                        ))

    return ()

pointRow
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t JumpTheGunLimit
    -> Dynamic t T.Text
    -> Dynamic t T.Text
    -> Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w earliest cTime cArrival utcOffset free dfNt pt tp sAltFs sAltAs x = do
    let tz = timeZone <$> utcOffset
    let pilot = fst <$> x
    let xB = snd <$> x

    let yAlt pilot' sAltFs' (_, Breakdown{total = p'}) =
            case Map.lookup pilot' sAltFs' of
                Nothing -> ("", "", "")
                Just
                    Alt.AltBreakdown
                        { place = nth
                        , total = p@(TaskPoints pts)
                        } -> (showRank nth, showRounded pts, showTaskPointsDiff p p')

    let yFs = ffor3 pilot sAltFs x yAlt
    let yAs = ffor3 pilot sAltAs x yAlt

    let yFsRank = ffor yFs $ \(yr, _, _) -> yr
    let yAsRank = ffor yAs $ \(yr, _, _) -> yr

    let yFsScore = ffor yFs $ \(_, ys, _) -> ys
    let yAsScore = ffor yAs $ \(_, ys, _) -> ys

    let yFsDiff = ffor yFs $ \(_, _, yd) -> yd
    let yAsDiff = ffor yAs $ \(_, _, yd) -> yd

    let xReach = reach <$> xB
    let points = breakdown . snd <$> x
    let v = velocity . snd <$> x
    let jtg = jump . snd <$> x

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let classEarly = ffor2 earliest jtg classOfEarlyStart

    let awardFree = ffor2 free xReach (\(MinimumDistance f) pd ->
            let c = "td-best-distance" in
            maybe
                (c, "")
                (\ReachToggle{extra = PilotDistance r} ->
                    if r >= f then (c, "") else
                       let c' = c <> " award-free"
                       in (c', T.pack $ printf "%.1f" f))
                pd)

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-norm td-placing" $ dynText yAsRank
        elClass "td" "td-norm td-placing" $ dynText yFsRank
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot
        elDynClass "td" classEarly . dynText $ showJumpedTheGunTime <$> jtg
        elClass "td" "td-start-start" . dynText $ (maybe "" . showSs) <$> tz <*> v
        elClass "td" "td-start-gate" . dynText $ (maybe "" . showGs) <$> tz <*> v
        elClass "td" "td-time-end" . dynText $ (maybe "" . showEs) <$> tz <*> v
        elClass "td" "td-time" . dynText $ maybe "" showGsVelocityTime <$> v
        elClass "td" "td-speed" . dynText $ maybe "" showVelocityVelocity <$> v

        elClass "td" "td-min-distance" . dynText $ snd <$> awardFree
        elDynClass "td" (fst <$> awardFree) . dynText
            $ maybe "" (showPilotDistance 1 . extra) <$> xReach

        elClass "td" "td-distance-points" . dynText
            $ showMax Pt.distance showTaskDistancePoints pt points
        elDynClass "td" cTime . dynText
            $ showMax Pt.time showTaskTimePoints pt points
        elClass "td" "td-leading-points" . dynText
            $ showMax Pt.leading showTaskLeadingPoints pt points
        elDynClass "td" cArrival . dynText
            $ showMax Pt.arrival showTaskArrivalPoints pt points

        elClass "td" "td-total-points" . dynText
            $ zipDynWith showTaskPointsRounded tp (total <$> xB)

        elClass "td" "td-norm td-total-points" $ dynText yFsScore
        elClass "td" "td-norm td-total-points" $ dynText yAsScore
        elClass "td" "td-norm td-total-points" $ dynText yFsDiff
        elClass "td" "td-norm td-total-points" $ dynText yAsDiff

dnfRows
    :: MonadWidget t m
    => Dynamic t Int
    -> TaskPlacing
    -> Dynamic t Dnf
    -> m ()
dnfRows w place ps' = do
    let ps = unDnf <$> ps'
    len <- sample . current $ length <$> ps
    let p1 = take 1 <$> ps
    let pN = drop 1 <$> ps

    case len of
        0 -> do
            return ()
        1 -> do
            _ <- simpleList ps (dnfRow w place (Just 1))
            return ()
        n -> do
            _ <- simpleList p1 (dnfRow w place (Just n))
            _ <- simpleList pN (dnfRow w place Nothing)
            return ()

dnfRow
    :: MonadWidget t m
    => Dynamic t Int
    -> TaskPlacing
    -> Maybe Int
    -> Dynamic t Pilot
    -> m ()
dnfRow w place rows pilot = do
    let dnfMajor =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: "15"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    let dnfMinor =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: "2"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-norm td-placing" $ text ""
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        dnfMajor
        elClass "td" "td-total-points" $ text "0"
        dnfMinor
        return ()
