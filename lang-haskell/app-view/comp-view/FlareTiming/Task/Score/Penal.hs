module FlareTiming.Task.Score.Penal (tableScorePenal) where

import Prelude hiding (min)
import Reflex.Dom
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
    , showTaskDistancePoints
    , showTaskArrivalPoints
    , showTaskLeadingPoints
    , showTaskTimePoints
    , showTaskPointsRounded
    , showTaskPointsNonZero
    , showTaskPointsDiff
    , showDemeritPointsNonZero
    , showRounded
    , showJumpedTheGunTime
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Penalty (PenaltySeqs(..), pprEffectiveAdd, pprEffectiveMul, pprEffectiveReset)
import WireTypes.Comp
    ( Discipline(..), EarlyStart(..), JumpTheGunLimit(..)
    , showEarlyStartEarliest, showEarlyStartPenaltyRate
    )
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot, classOfEarlyStart)
import FlareTiming.Task.Score.Show

tableScorePenal
    :: MonadWidget t m
    => Dynamic t Discipline
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
tableScorePenal hgOrPg early sgs _ln dnf' dfNt _vy vw _wg pt tp sDfs sEx = do
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

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text ""
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-early") . dynText
                    $ ((<> " of Jump-the-Gun") . showEarlyStartEarliest) <$> early
                elAttr "th" ("colspan" =: "5" <> "class" =: "th-points") $ dynText "Points Before Penalties Applied"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-demerit") $ text "Penalties ‡"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-points") $ text "Final Rounded Points"

            el "tr" $ do
                elClass "th" "th-norm th-placing" $ text "✓"
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-start-early" $ text "Early †"
                elClass "th" "th-early-demerit" $ text "Points"

                elClass "th" "th-distance-points" $ text "Distance"
                elDynClass "th" (fst <$> cTimePoints) $ text "Time"
                elClass "th" "th-leading-points" $ text "Lead"
                elDynClass "th" (fst <$> cArrivalPoints) $ text "Arrival"
                elClass "th" "th-total-points" $ text "Subtotal"
                elClass "th" "th-demerit-points" $ text "Frac"
                elClass "th" "th-demerit-points" $ text "Points"
                elClass "th" "th-demerit-points" $ text "Reset"
                elClass "th" "th-total-points" $ text "Total"
                elClass "th" "th-norm th-total-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                el "th" $ text ""
                elClass "th" "th-early-units" . dynText
                    $ ((\r -> "(" <> r <> ")") . showEarlyStartPenaltyRate) <$> early

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

                elAttr "th" ("colspan" =: "6") $ text ""

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

        let tdFoot = elAttr "td" ("colspan" =: "15")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† \"Early\" how much earlier than the start did this pilot jump the gun?"
            foot "‡ Fractional penalties are applied before point penalties. The effective point reductions are shown here."
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
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w earliest cTime cArrival dfNt pt tp sEx x = do
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
    let jtg = jump . snd <$> x
    let jtgPenalties = penaltiesJumpEffective . snd <$> x

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let classEarly = ffor2 earliest jtg classOfEarlyStart
    let jtgPenalty = ffor jtgPenalties (\PenaltySeqs{adds, muls, resets} ->
                        case (null adds, null muls, null resets) of
                            (True, True, True) -> ""
                            (False, True, True) -> pprEffectiveAdd 1 adds
                            (_, False, True) -> pprEffectiveMul 1 muls
                            (_, _, False) -> pprEffectiveReset 1 resets)

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-norm td-placing" $ dynText yRank
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot
        elDynClass "td" classEarly . dynText $ showJumpedTheGunTime <$> jtg
        elClass "td" "td-demerit-points" $ dynText jtgPenalty

        elClass "td" "td-distance-points" . dynText
            $ showMax Pt.distance showTaskDistancePoints pt points
        elDynClass "td" cTime . dynText
            $ showMax Pt.time showTaskTimePoints pt points
        elClass "td" "td-leading-points" . dynText
            $ showMax Pt.leading showTaskLeadingPoints pt points
        elDynClass "td" cArrival . dynText
            $ showMax Pt.arrival showTaskArrivalPoints pt points

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
