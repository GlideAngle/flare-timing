module FlareTiming.Task.Score.Distance (tableScoreDistance) where

import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..), TaskDistance(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.Point
    ( TaskPlacing(..)
    , TaskPoints(..)
    , Breakdown(..)
    , PilotDistance(..)
    , Points(..)
    , showDistancePoints
    , showDistancePointsDiff
    , showPilotDistance
    , showPilotDistanceDiff
    , showPilotAlt
    , showTaskLinearPoints
    , showTaskDifficultyPoints
    , showTaskDistancePoints
    )
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
tableScoreDistance utcOffset hgOrPg free sgs ln dnf' dfNt _vy vw _wg pt _tp sDfs sEx = do
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

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "2") $ text ""
                elAttr "th" ("colspan" =: "6" <> "class" =: "th-distance") $ text "Distance Flown"
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points-breakdown") $ text "Points for Distance"
                elAttr "th" ("colspan" =: "2") $ text ""

            el "tr" $ do
                elClass "th" "th-placing" $ text "#"
                elClass "th" "th-pilot" $ text "Pilot"
                elClass "th" "th-min-distance" $ text "Min"

                elClass "th" "th-best-distance" $ text "Reach †"
                elClass "th" "th-norm th-best-distance" $ text "✓-Reach"
                elClass "th" "th-norm th-diff" $ text "Δ-Reach"

                elClass "th" "th-alt-distance" $ text "Alt"
                elClass "th" "th-landed-distance" $ text "Landed"
                elClass "th" "th-reach-points" $ text "Reach ‡"
                elClass "th" "th-effort-points" $ text "Effort §"

                elClass "th" "th-distance-points" $ text "Subtotal"
                elClass "th" "th-norm th-distance-points" $ text "✓-Distance"
                elClass "th" "th-norm th-diff" $ text "Δ-Distance"

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

                thSpace
                thSpace

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        utcOffset
                        free
                        ln
                        dfNt
                        pt
                        (Map.fromList <$> sEx))

            dnfRows dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "14")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† How far along the course, reaching goal or elsewhere. The distance reached in the air can be further than the distance at landing."
            foot "‡ Points award for reach are also called linear distance points."
            foot "§ Points award for effort are also called distance difficulty points."
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
    => Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow _utcOffset free ln dfNt pt sEx x = do
    td <- sample . current $ (fmap . fmap) taskRoute ln
    MinimumDistance free' <- sample . current $ free

    let pilot = fst <$> x
    let xB = snd <$> x

    let alt = stoppedAlt <$> xB
    let reach = reachDistance <$> xB
    let points = breakdown <$> xB

    let classPilot = ffor2 pilot dfNt (\p (DfNoTrack ps) ->
                        let n = showPilotName p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let awardFree = ffor reach (\pd ->
            let c = ("td-best-distance", "td-landed-distance") in
            maybe
                (c, "")
                (\(PilotDistance r) ->
                    if r >= free' then (c, "") else
                       let c' =
                               ( fst c <> " award-free"
                               , snd c <> " award-free"
                               )

                       in (c', T.pack $ printf "%.1f" free'))
                pd)

    (yReach, yReachDiff, yDistance, yDistanceDiff) <- sample . current
                $ ffor3 pilot sEx x (\pilot' sEx' (_, Breakdown{reachDistance = dM', breakdown = Points{distance = d}}) ->
                case (td, Map.lookup pilot' sEx') of
                    (Just (TaskDistance dTask), Just Norm.NormBreakdown {distance = d', distanceFrac = dF}) ->
                        let r = dF * dTask
                            dR = PilotDistance $ max r free'
                            dM'' =
                                PilotDistance
                                . (\(PilotDistance r') -> max r' free')
                                <$> dM'
                        in
                            ( showPilotDistance dR
                            , maybe "" (showPilotDistanceDiff dR) dM''
                            , showDistancePoints d'
                            , showDistancePointsDiff d d'
                            )

                    _ -> ("", "", "", ""))

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
        elClass "td" "td-norm td-distance-points" . text $ yDistance
        elClass "td" "td-norm td-distance-points" . text $ yDistanceDiff

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
                        <> "colspan" =: "8"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ showPilotName <$> pilot
        dnfMega
        return ()
