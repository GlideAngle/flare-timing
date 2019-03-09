module FlareTiming.Task.Score.Distance (tableScoreDistance) where

import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (pack)
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
    , PilotDistance(..)
    , showPilotDistance
    , showPilotDistanceDiff
    , showPilotAlt
    , showTaskLinearPoints
    , showTaskDifficultyPoints
    , showTaskDistancePoints
    , showDistanceWeight
    )
import WireTypes.Validity (showLaunchValidity, showDistanceValidity)
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..))
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Task.Score.Show

tableScoreDistance
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
tableScoreDistance utcOffset hgOrPg free sgs _ln dnf' dfNt vy vw wg pt _tp sDfs sEx = do
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

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("rowspan" =: "2" <> "class" =: "th-placing") $ text "#"
                elAttr "th" ("rowspan" =: "2" <> "class" =: "th-pilot") $ text "Pilot"
                elAttr "th" ("colspan" =: "6" <> "class" =: "th-distance") $ text "Distance Flown"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points-breakdown") $ text "Points for Distance"

            el "tr" $ do
                elClass "th" "th-min-distance" $ text "Min"

                elClass "th" "th-best-distance" $ text "Reach †"
                elClass "th" "th-norm th-best-distance" $ text "Reach"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-alt-distance" $ text "Alt"
                elClass "th" "th-landed-distance" $ text "Landed"
                elClass "th" "th-reach-points" $ text "Reach ‡"
                elClass "th" "th-effort-points" $ text "Effort §"

                elClass "th" "th-distance-points" $ text "Subtotal"

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

                elClass "th" "th-distance-validity" . dynText $
                    maybe
                        ""
                        ( showDistanceValidity
                        . Vy.distance
                        )
                    <$> vy

            elClass "tr" "tr-weight" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-weight") $ text "Weights"
                elAttr "th" ("colspan" =: "8") $ text ""

                elClass "th" "th-distance-weight" . dynText $
                    maybe
                        ""
                        ( showDistanceWeight
                        . Wg.distance
                        )
                    <$> wg

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elClass "th" "th-min-distance-units" $ text "(km)"
                elClass "th" "th-best-distance-units" $ text "(km)"
                elClass "th" "th-best-distance-units" $ text "(km)"
                elClass "th" "th-best-distance-units" $ text "(km)"
                elClass "th" "th-alt-distance-units" $ text "(m)"
                elClass "th" "th-landed-distance-units" $ text "(km)"

                elClass "th" "th-reach-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLinearPoints (Just x) x)
                        . Pt.reach
                        )
                    <$> pt

                elClass "th" "th-effort-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDifficultyPoints (Just x) x)
                        . Pt.effort
                        )
                    <$> pt

                elClass "th" "th-distance-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDistancePoints (Just x) x)
                        . Pt.distance
                        )
                    <$> pt

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        utcOffset
                        free
                        dfNt
                        pt
                        (Map.fromList <$> sEx))

            dnfRows dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "12")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† How far along the course, reaching goal or elsewhere. The distance reached in the air can be further than the distance at landing."
            foot "‡ Points award for reach are also called linear distance points."
            foot "§ Points award for effort are also called distance difficulty points."
            foot "‖ \"Time\" is the time across the speed section from time zero of the start gate taken."
            foot "¶ \"Pace\" is the time across the speed section from the time of crossing the start for the last time."
            foot "☞ Pilots without a tracklog but given a distance by the scorer."
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
    => Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow _utcOffset free dfNt pt sEx x = do
    let pilot = fst <$> x
    let xB = snd <$> x

    let alt = stoppedAlt <$> xB
    let reach = reachDistance <$> xB
    let points = breakdown . snd <$> x

    let classPilot = ffor2 pilot dfNt (\p (DfNoTrack ps) ->
                        let n = showPilotName p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let awardFree = ffor2 free reach (\(MinimumDistance f) pd ->
            let c = ("td-best-distance", "td-landed-distance") in
            maybe
                (c, "")
                (\(PilotDistance r) ->
                    if r >= f then (c, "") else
                       let c' =
                               ( fst c <> " award-free"
                               , snd c <> " award-free"
                               )

                       in (c', T.pack $ printf "%.1f" f))
                pd)

    (yReach, yReachDiff) <- sample . current
                $ ffor3 pilot sEx x (\pilot' sEx' (_, Breakdown{reachDistance = dM'}) ->
                case Map.lookup pilot' sEx' of
                    Just Norm.NormBreakdown {distanceMade = dM} ->
                            ( showPilotDistance dM
                            , maybe "" (showPilotDistanceDiff dM) dM'
                            )

                    _ -> ("", ""))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-min-distance" . dynText $ snd <$> awardFree

        elDynClass "td" (fst . fst <$> awardFree) . dynText
            $ maybe "" showPilotDistance <$> reach
        elClass "td" "td-norm td-best-distance" $ text yReach
        elClass "td" "td-norm td-diff" $ text yReachDiff

        elClass "td" "td-alt-distance" . dynText
            $ maybe "" showPilotAlt <$> alt
        elDynClass "td" (snd . fst <$> awardFree) . dynText
            $ maybe "" showPilotDistance . landedDistance <$> xB

        elClass "td" "td-reach-points" . dynText
            $ showMax Pt.reach showTaskLinearPoints pt points
        elClass "td" "td-effort-points" . dynText
            $ showMax Pt.effort showTaskDifficultyPoints pt points
        elClass "td" "td-distance-points" . dynText
            $ showMax Pt.distance showTaskDistancePoints pt points

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
    let dnfMega =
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
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ showPilotName <$> pilot
        dnfMega
        return ()
