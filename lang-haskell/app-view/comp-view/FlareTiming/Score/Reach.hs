module FlareTiming.Score.Reach (tableScoreReach) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Bk (Breakdown(..))
import WireTypes.Point
    ( TaskPlacing(..), PilotDistance(..), ReachToggle(..)
    , showPilotDistance, showTaskLinearPoints, cmpReach
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..), TaskStop(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Score.Show

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
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> m ()
tableScoreReach utcOffset hgOrPg free sgs ln stp dnf' dfNt vw pt sDfs _sAltFs = do
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
                                   then ("15", "4")
                                   else ("14", "3"))

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
                        elClass "th" "th-stopped" $
                            text "Extra reach via glide, only with stopped tasks"
                    Nothing ->
                        el "th" $ text "")

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
                    Nothing -> do
                        elClass "th" "th-distance-flown" $ text "Flown †")

                elClass "th" "th-reach-points" $ text "Reach ¶"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elClass "th" "th-min-distance-units" $ text "(km)"

                dyn_ $ ffor stp (\case
                    Just _ -> do
                        elClass "th" "th-best-distance-units" $ text "(km)"
                        elClass "th" "th-best-distance-units" $ text "(km)"
                    Nothing -> do
                        elClass "th" "th-best-distance-units" $ text "(km)")

                elClass "th" "th-reach-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLinearPoints (Just x) x)
                        . Pt.reach
                        )
                    <$> pt

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    (sortBy cmpReach <$> sDfs)
                    (pointRow w utcOffset free ln stp dfNt pt)

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
    -> Dynamic t (Pilot, Bk.Breakdown)
    -> m ()
pointRow w _utcOffset free _ln stp dfNt pt x = do
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

    (xF, xE) <- sample . current $ ffor x (\(_, Bk.Breakdown{reach}) ->
                    fromMaybe ("", "") $ do
                        ReachToggle{extra = rE, flown = rF} <- reach
                        return
                            ( showPilotDistance 3 rF
                            , showPilotDistance 3 rE
                            ))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-placing" . dynText $ showRank . Bk.place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-min-distance" . dynText $ snd <$> awardFree

        dyn_ $ ffor stp (\case
            Just _ -> do
                elDynClass "td" (fst <$> awardFree) $ text xF
                elDynClass "td" (fst <$> awardFree) $ text xE
            Nothing -> do
                elDynClass "td" (fst <$> awardFree) $ text xF)

        elClass "td" "td-reach-points" . dynText
            $ showMax Pt.reach showTaskLinearPoints pt points

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
