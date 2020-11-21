module FlareTiming.Task.Penal.EssGoal (tablePenalEssGoal) where

import Prelude hiding (min)
import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.Point
    ( TaskPlacing(..)
    , TaskPoints(..)
    , Breakdown(..)
    , EssNotGoal(..)
    , showTaskArrivalPoints
    , showTaskTimeArrivalPoints
    , showTaskTimePoints
    , showTaskPointsRounded
    , showTaskPointsNonZero
    , showTaskPointsDiff
    , showDemeritPointsNonZero
    , showRounded
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Penalty (PenaltySeqs(..), pprEffectiveAdd, pprEffectiveMul, pprEffectiveReset)
import WireTypes.Comp
    ( Discipline(..), EarlyStart(..), JumpTheGunLimit(..), EGwScaling(..), Tweak(..)
    , showEarlyStartPenaltyRate
    )
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Task.Score.Show

tablePenalEssGoal
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t EarlyStart
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
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> m ()
tablePenalEssGoal hgOrPg tweak early sgs _ln dnf' dfNt _vy vw _wg pt tp sDfs sEx = do
    let w = ffor sDfs (pilotIdsWidth . fmap fst)
    let dnf = unDnf <$> dnf'
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenDfs :: Int <- sample . current $ length <$> sDfs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenDfs + 1

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

    let egScale = ffor tweak $
                    maybe
                        ("ESS not goal => no time validity", "")
                        (\Tweak{essNotGoalScaling = EGwScaling x} ->
                            ( T.pack $ printf "ESS not goal => %.1f time validity" x
                            , T.pack $ printf "%.1f*TV" (1 - x)))

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "6") $ text ""
                elAttr "th" ("colspan" =: "4" <> "class" =: "th-points") $ dynText "Points Before Penalties Applied"
                elAttr "th" ("colspan" =: "6") $ text ""

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text ""
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-early") . dynText
                    $ fst <$> egScale

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-time-arrival-points") $ dynText "Time + Arrival ¶"
                elClass "th" "th-points" $ text ""
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-demerit") $ text "Penalties ‡"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-points") $ text "Final Rounded Points"

            el "tr" $ do
                elClass "th" "th-norm th-placing" $ text "✓"
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-zone-ess" $ text "ESS"
                elClass "th" "th-zone-goal" $ text "Goal"
                elClass "th" "th-early-demerit" $ text "Points"

                elClass "th" "th-time-arrival-points" $ text ""
                elDynClass "th" (fst <$> cTimePoints) $ text "Time"
                elDynClass "th" (fst <$> cArrivalPoints) $ text "Arrival"
                elClass "th" "th-total-points" $ text "Subtotal"
                elClass "th" "th-demerit-points" $ text "Frac"
                elClass "th" "th-demerit-points" $ text "Points"
                elClass "th" "th-demerit-points" $ text "Reset"
                elClass "th" "th-total-points" $ text "Total"
                elClass "th" "th-norm th-total-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "5" <> "class" =: "th-allocation") $ text "Available Points (Units)"

                elClass "th" "th-early-units" . dynText $ snd <$> egScale

                elClass "th" "th-early-units" $ text "TV"

                elClass "th" "th-time-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskTimePoints (Just x) x)
                        . Pt.time
                        )
                    <$> pt

                elClass "th" "th-arrival-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskArrivalPoints (Just x) x)
                        . Pt.arrival
                        )
                    <$> pt

                elAttr "th" ("colspan" =: "4") $ text ""

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPointsRounded (Just x) x)
                    <$> tp

                elAttr "th" ("colspan" =: "2") $ text ""

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        w
                        (earliest <$> early)
                        (snd <$> cTimePoints)
                        (snd <$> cArrivalPoints)
                        dfNt
                        pt
                        tp
                        (Map.fromList <$> sEx))

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "16")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† \"Early\" how much earlier than the start did this pilot jump the gun?"
            foot "‡ Fractional penalties are applied before point penalties. The effective point reductions are shown here."
            foot "☞ Pilots without a tracklog but given a distance by the scorer."
            foot "✓ An expected value as calculated by the official scoring program, FS."
            foot "Δ A difference between a value and an expected value."
            foot "¶ The time and arrival points are time validated (TV) by making goal."
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
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w _earliest cTime cArrival dfNt pt tp sEx x = do
    let pilot = fst <$> x
    let xB = snd <$> x
    let y = ffor3 pilot sEx x (\pilot' sEx' (_, Breakdown{total = p'}) ->
                case Map.lookup pilot' sEx' of
                    Nothing -> ("", "", "")
                    Just
                        Norm.NormBreakdown
                            { place = nth
                            , total = p@(TaskPoints pts)
                            } -> (showRank nth, showRounded pts, showTaskPointsDiff p p'))

    let yRank = ffor y $ \(yr, _, _) -> yr
    let yScore = ffor y $ \(_, ys, _) -> ys
    let yDiff = ffor y $ \(_, _, yd) -> yd

    let points = breakdown . snd <$> x
    let egPenalties = penaltiesEssNotGoal . snd <$> x

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let egPenalty = ffor egPenalties (\PenaltySeqs{adds, muls, resets} ->
                        case (null adds, null muls, null resets) of
                            (True, True, True) -> ""
                            (False, True, True) -> pprEffectiveAdd 1 adds
                            (_, False, True) -> pprEffectiveMul 1 muls
                            (_, _, False) -> pprEffectiveReset 1 resets)

    let eg = ffor x (\(_, Breakdown{essNotGoal}) ->
                case essNotGoal of
                    Nothing ->
                        ( el "td" $ text ""
                        , el "td" $ text ""
                        )

                    Just (EssNotGoal False) ->
                        ( elClass "td" "td-zone-ess td-zone-made" $ text "✓"
                        , elClass "td" "td-zone-goal td-zone-made" $ text "✓"
                        )

                    Just (EssNotGoal True) ->
                        ( elClass "td" "td-zone-ess td-zone-made" $ text "✓"
                        , elClass "td" "td-zone-goal td-zone-miss" $ text "✗"
                        ))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-norm td-placing" $ dynText yRank
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot
        dyn_ $ fst <$> eg
        dyn_ $ snd <$> eg
        elClass "td" "td-demerit-points" $ dynText egPenalty

        dyn_ $ ffor x (\(_, Breakdown{essNotGoal}) ->
            case essNotGoal of
                Nothing -> do
                    elClass "td" "td-time-arrival-points" $ text ""
                    elDynClass "td" cTime $ text ""
                    elDynClass "td" cArrival $ text ""

                Just (EssNotGoal False) -> do
                    elClass "td" "td-time-arrival-points" $ text ""
                    elDynClass "td" cTime $ text ""
                    elDynClass "td" cArrival $ text ""

                Just (EssNotGoal True) -> do
                    elClass "td" "td-time-arrival-points" . dynText
                        $ showMax
                            (\ptsTask -> (Pt.time ptsTask, Pt.arrival ptsTask))
                            showTaskTimeArrivalPoints
                            pt
                            points

                    elDynClass "td" cTime . dynText
                        $ showMax Pt.time showTaskTimePoints pt points
                    elDynClass "td" cArrival . dynText
                        $ showMax Pt.arrival showTaskArrivalPoints pt points)

        elClass "td" "td-total-points" . dynText
            $ (showTaskPointsNonZero 1 . subtotal) <$> xB

        elClass "td" "td-demerit-points frac" . dynText
            $ (showDemeritPointsNonZero 1 . demeritFrac) <$> xB

        elClass "td" "td-demerit-points points" . dynText
            $ (showDemeritPointsNonZero 1 . demeritPoint) <$> xB

        elClass "td" "td-demerit-points reset" . dynText
            $ (showDemeritPointsNonZero 1 . demeritReset) <$> xB

        elClass "td" "td-total-points" . dynText
            $ zipDynWith showTaskPointsRounded tp (total <$> xB)

        elClass "td" "td-norm td-total-points" $ dynText yScore
        elClass "td" "td-norm td-total-points" $ dynText yDiff

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
                        <> "colspan" =: "6"
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
                        <> "colspan" =: "6"
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
