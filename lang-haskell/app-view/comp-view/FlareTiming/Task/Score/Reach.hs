module FlareTiming.Task.Score.Reach (tableScoreReach) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Bk (Breakdown(..))
import WireTypes.Point
    ( TaskPlacing(..)
    , PilotDistance(..)
    , Points(..)
    , ReachToggle(..)
    , LinearPoints(..), showLinearPoints, showLinearPointsDiff
    , DifficultyPoints(..)
    , DistancePoints(..)
    , showPilotDistance, showPilotDistanceDiff
    , showTaskLinearPoints
    , cmpReach
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..), TaskStop(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Task.Score.Show

tableScoreReach
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Discipline
    -> Dynamic t MinimumDistance
    -> Dynamic t [Pt.StartGate]
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t (Maybe TaskStop)
    -> Dynamic t Dnf
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t [(Pilot, Bk.Breakdown)]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> m ()
tableScoreReach utcOffset hgOrPg free sgs ln stp dnf' dfNt vw pt sDfs sEx = do
    let w = ffor sDfs (pilotIdsWidth . fmap fst)
    let dnf = unDnf <$> dnf'
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenDfs :: Int <- sample . current $ length <$> sDfs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenDfs + 1

    (cols, colsDnfPad) <- sample . current $ ffor stp (\s ->
                                if isJust s
                                   then ("19", "8")
                                   else ("18", "7"))

    let thSpace = elClass "th" "th-space" $ text ""

    let tableClass =
            let tc = "table is-striped is-narrow is-fullwidth" in
            ffor2 hgOrPg sgs (\x gs ->
                let y = T.pack . show $ x in
                y <> (if null gs then " " else " sg ") <> tc)

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do


            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text ""

                dyn_ $ ffor stp (\case
                    Just _ -> do
                        el "th" $ text ""
                        elAttr "th" ("colspan" =: "3" <> "class" =: "th-stopped") $
                            text "Only with Stopped Tasks"
                    Nothing ->
                        elAttr "th" ("colspan" =: "3") $ text "")

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points-breakdown") $
                    text "Points for Reach (Descending)"

            el "tr" $ do
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-min-distance" $ text "Min"

                dyn_ $ ffor stp (\case
                    Just _ -> do
                        elClass "th" "th-distance-flown" $ text "Flown †"
                        elClass "th" "th-distance-extra" $ text "Extra ‡"
                        elClass "th" "th-norm th-best-distance" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"
                    Nothing -> do
                        elClass "th" "th-distance-flown" $ text "Flown †"
                        elClass "th" "th-norm th-best-distance" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ")

                elClass "th" "th-reach-points" $ text "Reach ¶"
                elClass "th" "th-norm th-reach-points" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elClass "th" "th-min-distance-units" $ text "(km)"

                dyn_ $ ffor stp (\case
                    Just _ -> do
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)"
                    Nothing -> do
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)")

                elClass "th" "th-reach-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLinearPoints (Just x) x)
                        . Pt.reach
                        )
                    <$> pt

                thSpace
                thSpace


        _ <- el "tbody" $ do
            _ <-
                simpleList
                    (sortBy cmpReach <$> sDfs)
                    (pointRow
                        w
                        utcOffset
                        free
                        ln
                        stp
                        dfNt
                        pt
                        (Map.fromList <$> sEx))

            dnfRows colsDnfPad w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: cols)
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† How far along the course, reaching goal or elsewhere. The distance reached in the air can be further than the distance at landing."
            foot "‡ With altitude above goal converted to extra reach via glide."
            foot "¶ Points awarded for reach are also called linear distance points."
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
    -> Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t (Maybe TaskStop)
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Map.Map Pilot Norm.NormBreakdown)
    -> Dynamic t (Pilot, Bk.Breakdown)
    -> m ()
pointRow w _utcOffset free _ln stp dfNt pt sEx x = do
    MinimumDistance free' <- sample . current $ free

    let pilot = fst <$> x
    let xB = snd <$> x

    let extraReach = fmap extra . Bk.reach <$> xB
    let points = Bk.breakdown <$> xB

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    let awardFree = ffor extraReach (\pd ->
            let c = "td-best-distance" in
            maybe
                (c, "")
                (\(PilotDistance r) ->
                    if r >= free' then (c, "") else
                    (c <> " award-free", T.pack $ printf "%.1f" free'))
                pd)

    (xF, xE
        , yF, yDiffF
        , yE, yDiffE
        , rPts, rPtsDiff) <- sample . current
                $ ffor3 pilot sEx x (\pilot' sEx' (_, Bk.Breakdown
                                                        { reach
                                                        , breakdown =
                                                            Points
                                                                { reach = rPts
                                                                }
                                                        }
                                                  ) ->
                    fromMaybe ("", "", "", "", "", "", "", "") $ do
                        Norm.NormBreakdown
                            { breakdown =
                                Points
                                    { reach = rPtsN
                                    , effort = ePtsN
                                    , distance = dPtsN
                                    }
                            , reach =
                                ReachToggle
                                    { flown = rFN
                                    , extra = rEN
                                    }
                            } <- Map.lookup pilot' sEx'
                        ReachToggle{extra = rE, flown = rF} <- reach

                        let quieten s =
                                case (rPtsN, ePtsN, dPtsN) of
                                    (LinearPoints 0, DifficultyPoints 0, DistancePoints 0) -> s
                                    (LinearPoints 0, DifficultyPoints 0, _) -> ""
                                    _ -> s

                        return
                            ( showPilotDistance 3 rF
                            , showPilotDistance 3 rE
                            , showPilotDistance 3 rFN
                            , showPilotDistanceDiff 3 rFN rF
                            , showPilotDistance 3 rEN
                            , showPilotDistanceDiff 3 rEN rE
                            , quieten $ showLinearPoints rPtsN
                            , quieten $ showLinearPointsDiff rPtsN rPts
                            ))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-placing" . dynText $ showRank . Bk.place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-min-distance" . dynText $ snd <$> awardFree

        dyn_ $ ffor stp (\case
            Just _ -> do
                elDynClass "td" (fst <$> awardFree) $ text xF
                elDynClass "td" (fst <$> awardFree) $ text xE
                elClass "td" "td-norm td-best-distance" $ text yE
                elClass "td" "td-norm td-diff" $ text yDiffE
            Nothing -> do
                elDynClass "td" (fst <$> awardFree) $ text xF
                elClass "td" "td-norm td-best-distance" $ text yF
                elClass "td" "td-norm td-diff" $ text yDiffF)

        elClass "td" "td-reach-points" . dynText
            $ showMax Pt.reach showTaskLinearPoints pt points
        elClass "td" "td-norm td-reach-points" . text $ rPts
        elClass "td" "td-norm td-reach-points" . text $ rPtsDiff

dnfRows
    :: MonadWidget t m
    => T.Text -- ^ colspan
    -> Dynamic t Int
    -> TaskPlacing
    -> Dynamic t Dnf
    -> m ()
dnfRows c w place ps' = do
    let ps = unDnf <$> ps'
    len <- sample . current $ length <$> ps
    let p1 = take 1 <$> ps
    let pN = drop 1 <$> ps

    case len of
        0 -> do
            return ()
        1 -> do
            _ <- simpleList ps (dnfRow c w place (Just 1))
            return ()
        n -> do
            _ <- simpleList p1 (dnfRow c w place (Just n))
            _ <- simpleList pN (dnfRow c w place Nothing)
            return ()

dnfRow
    :: MonadWidget t m
    => T.Text -- ^ colspan
    -> Dynamic t Int
    -> TaskPlacing
    -> Maybe Int
    -> Dynamic t Pilot
    -> m ()
dnfRow colsDnfPad w place rows pilot = do
    let dnfMega =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: colsDnfPad
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        dnfMega
        return ()
