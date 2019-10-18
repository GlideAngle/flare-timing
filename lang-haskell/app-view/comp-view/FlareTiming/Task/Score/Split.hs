module FlareTiming.Task.Score.Split (tableScoreSplit) where

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
    , Points(..)

    , showDistancePoints
    , showDistancePointsDiff
    , showLeadingPoints
    , showLeadingPointsDiff
    , showArrivalPoints
    , showArrivalPointsDiff
    , showTimePoints
    , showTimePointsDiff

    , showTaskDistancePoints
    , showTaskArrivalPoints
    , showTaskLeadingPoints
    , showTaskTimePoints
    , showTaskPoints
    , showTaskPointsDiff
    , showRounded

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
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..))
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot)
import FlareTiming.Task.Score.Show

tableScoreSplit
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Discipline
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
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> m ()
tableScoreSplit utcOffset hgOrPg free sgs _ln dnf' dfNt vy vw wg pt tp sDfs sEx = do
    let dnf = unDnf <$> dnf'
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenDfs :: Int <- sample . current $ length <$> sDfs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenDfs + 1

    let thSpace = elClass "th" "th-space" $ text ""
    let thNorm = elClass "th" "th-norm" $ text ""

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

            elClass "tr" "tr-validity" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-launch-validity") . dynText $
                    maybe
                        ""
                        ( (\v ->
                            "Validity (Launch = "
                            <> showLaunchValidity v
                            <> ")")
                        . Vy.launch
                        )
                    <$> vy

                elClass "th" "th-distance-validity" . dynText $
                    maybe
                        ""
                        ( showDistanceValidity
                        . Vy.distance
                        )
                    <$> vy

                thNorm
                thNorm

                elClass "th" "th-time-validity" . dynText $
                    maybe
                        ""
                        ( showTimeValidity
                        . Vy.time
                        )
                    <$> vy

                thNorm
                thNorm
                thSpace
                thNorm
                thNorm
                thSpace
                thNorm
                thNorm

                elClass "th" "th-task-validity" . dynText $
                    maybe
                        ""
                        ( showTaskValidity
                        . Vy.task
                        )
                    <$> vy

                thNorm
                thNorm

            elClass "tr" "tr-weight" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-weight") $ text "Weights"

                elClass "th" "th-distance-weight" . dynText $
                    maybe
                        ""
                        ( showDistanceWeight
                        . Wg.distance
                        )
                    <$> wg

                thNorm
                thNorm

                elClass "th" "th-time-weight" . dynText$
                    maybe
                        ""
                        ( showTimeWeight
                        . Wg.time
                        )
                    <$> wg

                thNorm
                thNorm

                elClass "th" "th-leading-weight" . dynText$
                    maybe
                        ""
                        ( showLeadingWeight
                        . Wg.leading
                        )
                    <$> wg

                thNorm
                thNorm

                elClass "th" "th-arrival-weight" . dynText$
                    maybe
                        ""
                        ( showArrivalWeight
                        . Wg.arrival
                        )
                    <$> wg

                thNorm
                thNorm
                thSpace
                thNorm
                thNorm

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-allocation") $ text "Available Points"

                elClass "th" "th-distance-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDistancePoints (Just x) x)
                        . Pt.distance
                        )
                    <$> pt

                thNorm
                thNorm

                elClass "th" "th-time-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskTimePoints (Just x) x)
                        . Pt.time
                        )
                    <$> pt

                thNorm
                thNorm

                elClass "th" "th-leading-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLeadingPoints (Just x) x)
                        . Pt.leading
                        )
                    <$> pt

                thNorm
                thNorm

                elClass "th" "th-arrival-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskArrivalPoints (Just x) x)
                        . Pt.arrival
                        )
                    <$> pt

                thNorm
                thNorm

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPoints (Just x) x)
                    <$> tp

                thNorm
                thNorm

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text ""

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points-breakdown")
                    $ text "Distance"

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-time-points") $ text "Time"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-leading-points") $ text "Lead"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-arrival-points") $ text "Arrival"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-total-points") $ text "Total"

            el "tr" $ do
                elClass "th" "th-norm th-placing" $ text "✓"
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" $ text "###-Pilot"

                elClass "th" "th-distance-points" $ text ""
                elClass "th" "th-norm th-distance-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elDynClass "th" (fst <$> cTimePoints) $ text ""
                elClass "th" "th-norm th-time-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-leading-points" $ text ""
                elClass "th" "th-norm th-leading-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elDynClass "th" (fst <$> cArrivalPoints) $ text ""
                elClass "th" "th-norm th-arrival-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-total-points" $ text ""
                elClass "th" "th-norm th-total-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        (snd <$> cTimePoints)
                        (snd <$> cArrivalPoints)
                        utcOffset
                        free
                        dfNt
                        pt
                        tp
                        (Map.fromList <$> sEx))

            dnfRows dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "21")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
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
    => Dynamic t T.Text
    -> Dynamic t T.Text
    -> Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow cTime cArrival _utcOffset _free dfNt pt tp sEx x = do
    let pilot = fst <$> x
    let xB = snd <$> x

    (yRank, yScore, yDiff, yDistance, yDistanceDiff, yLeading, yLeadingDiff, yArrival, yArrivalDiff, yTime, yTimeDiff) <- sample . current
                $ ffor3 pilot sEx x (\pilot' sEx' (_, Breakdown{total = p', breakdown = Points{distance = d', leading = l', arrival = a', time = t'}}) ->
                case Map.lookup pilot' sEx' of
                    Nothing -> ("", "", "", "", "", "", "", "", "", "", "")
                    Just
                        Norm.NormBreakdown
                            { place = nth
                            , total = p@(TaskPoints pts)
                            , breakdown =
                                Points
                                    { distance = d
                                    , leading = l
                                    , arrival = a
                                    , time = t
                                    }
                            } ->
                        ( showRank nth
                        , showRounded pts
                        , showTaskPointsDiff p p'
                        , showDistancePoints d
                        , showDistancePointsDiff d d'
                        , showLeadingPoints l
                        , showLeadingPointsDiff l l'
                        , showArrivalPoints a
                        , showArrivalPointsDiff a a'
                        , showTimePoints t
                        , showTimePointsDiff t t'
                        ))

    let points = breakdown . snd <$> x

    let classPilot = ffor2 pilot dfNt (\p (DfNoTrack ps) ->
                        let n = showPilot p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-norm td-placing" . text $ yRank
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-distance-points" . dynText
            $ showMax Pt.distance showTaskDistancePoints pt points
        elClass "td" "td-norm td-distance-points" . text $ yDistance
        elClass "td" "td-norm td-distance-points" . text $ yDistanceDiff

        elDynClass "td" cTime . dynText
            $ showMax Pt.time showTaskTimePoints pt points
        elClass "td" "td-norm td-time-points" . text $ yTime
        elClass "td" "td-norm td-time-points" . text $ yTimeDiff

        elClass "td" "td-leading-points" . dynText
            $ showMax Pt.leading showTaskLeadingPoints pt points
        elClass "td" "td-norm td-leading-points" . text $ yLeading
        elClass "td" "td-norm td-leading-points" . text $ yLeadingDiff

        elDynClass "td" cArrival . dynText
            $ showMax Pt.arrival showTaskArrivalPoints pt points
        elClass "td" "td-norm td-arrival-points" . text $ yArrival
        elClass "td" "td-norm td-arrival-points" . text $ yArrivalDiff

        elClass "td" "td-total-points" . dynText
            $ zipDynWith showTaskPoints tp (total <$> xB)

        elClass "td" "td-norm td-total-points" . text $ yScore
        elClass "td" "td-norm td-total-points" . text $ yDiff

dnfRows
    :: MonadWidget t m
    => TaskPlacing
    -> Dynamic t Dnf
    -> m ()
dnfRows place ps' = do
    let ps = unDnf <$> ps'
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
    let dnfMajor =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: "12"
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
        elClass "td" "td-pilot" . dynText $ showPilot <$> pilot
        dnfMajor
        elClass "td" "td-total-points" $ text "0"
        dnfMinor
        return ()
