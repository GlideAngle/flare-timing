module FlareTiming.VieScoreBoth.Over (tableVieScoreBothOver) where

import Prelude hiding (min)
import Reflex.Dom
import qualified Data.Text as T (pack)
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
    , showTaskPointsRounded
    , showTaskPointsDiff
    , showTaskPointsDiffStats
    , showRounded
    )
import WireTypes.ValidityWorking (ValidityWorking(..))
import WireTypes.Comp
    ( UtcOffset(..), Discipline(..), MinimumDistance(..), EarlyStart(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Score.Show

tableVieScoreBothOver
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
tableVieScoreBothOver _utcOffset hgOrPg _early _free sgs _ln dnf' dfNt _vy _vw _wg _pt tp sDfs sAltFs sAltAs = do
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
            let tc = "table is-striped is-narrow" in
            ffor2 hgOrPg sgs (\x gs ->
                let y = T.pack . show $ x in
                y <> (if null gs then " " else " sg ") <> tc)

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

    let stats = ffor
                        yDiff
                        ((\(fs', as', ft) ->
                            let fs = sequence fs'

                                -- NOTE: For some pilots, airScore is giving
                                -- a null score. To do the stats, substitute
                                -- zero for those scores.
                                as = Just $ maybe (TaskPoints 0) id <$> as'

                                dFtFs = showTaskPointsDiffStats fs ft
                                dFtAs = showTaskPointsDiffStats as ft
                                dAsFs = maybe "" (showTaskPointsDiffStats fs) as
                            in
                                (dFtFs, (dFtAs, dAsFs)))
                        . unzip3)

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "4") $ text ""
                elAttr "th" ("colspan" =: "12" <> "class" =: "ft-fs th-points")
                    $ text "Total Point Comparisons"

            el "tr" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-place") $ text "Place"
                thSpace
                elAttr "th" ("colspan" =: "3" <> "class" =: "ft-fs th-points") . dynText
                    $ fst <$> stats
                elAttr "th" ("colspan" =: "3" <> "class" =: "as-ft th-points") . dynText
                    $ fst . snd <$> stats
                elAttr "th" ("colspan" =: "3" <> "class" =: "as-fs th-points") . dynText
                    $ snd . snd <$> stats

            el "tr" $ do
                elClass "th" "as th-norm th-placing" $ text "As"
                elClass "th" "fs th-norm th-placing" $ text "Fs"
                elClass "th" "ft th-placing" $ text "Ft"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot

                elClass "th" "th-total-points" $ text "Ft"
                elClass "th" "fs th-norm th-total-points" $ text "Fs"
                elClass "th" "th-diff" $ text "Δ Ft-Fs"

                elClass "th" "th-total-points" $ text "Ft"
                elClass "th" "as th-norm th-total-points" $ text "As"
                elClass "th" "th-diff" $ text "Δ Ft-As"

                elClass "th" "as th-norm th-total-points" $ text "As"
                elClass "th" "fs th-norm th-total-points" $ text "Fs"
                elClass "th" "th-diff" $ text "Δ As-Fs"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "4" <> "class" =: "th-allocation") $ text "Available Points"

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPointsRounded (Just x) x)
                    <$> tp

                thSpace
                thSpace
                thSpace
                thSpace
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
                        dfNt
                        tp
                        (Map.fromList <$> sAltFs)
                        (Map.fromList <$> sAltAs))

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "13")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "☞ Pilots without a tracklog but given a distance by the scorer."
            foot "Δ A difference between total points, mean ± standard deviation."

    return ()

pointRow
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w dfNt tp sAltFs sAltAs x = do
    let pilot = fst <$> x
    let xB = snd <$> x

    let yAlt pilot' sAltFs' (_, Breakdown{total = pFt}) =
            case Map.lookup pilot' sAltFs' of
                Nothing -> ("", "", "")
                Just
                    Alt.AltBreakdown
                        { place = nth
                        , total = pFs@(TaskPoints pts)
                        } -> (showRank nth, showRounded pts, showTaskPointsDiff pFs pFt)

    let zAlt pilot' sAltAs' sAltFs' =
            case (Map.lookup pilot' sAltAs', Map.lookup pilot' sAltFs') of
                (Nothing, _) -> ""
                (_, Nothing) -> ""
                (Just Alt.AltBreakdown{total = pAs}, Just Alt.AltBreakdown{total = pFs}) ->
                    showTaskPointsDiff pFs pAs

    let yFs = ffor3 pilot sAltFs x yAlt
    let yAs = ffor3 pilot sAltAs x yAlt
    let zAsFsDiff = ffor3 pilot sAltAs sAltFs zAlt

    let yFsRank = ffor yFs $ \(yr, _, _) -> yr
    let yAsRank = ffor yAs $ \(yr, _, _) -> yr

    let yFsScore = ffor yFs $ \(_, ys, _) -> ys
    let yAsScore = ffor yAs $ \(_, ys, _) -> ys

    let yFtFsDiff = ffor yFs $ \(_, _, yd) -> yd
    let yFtAsDiff = ffor yAs $ \(_, _, yd) -> yd

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "as td-norm td-placing" $ dynText yAsRank
        elClass "td" "fs td-norm td-placing" $ dynText yFsRank
        elClass "td" "ft td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-total-points" . dynText
            $ zipDynWith showTaskPointsRounded tp (total <$> xB)
        elClass "td" "fs td-norm td-total-points" $ dynText yFsScore
        elClass "td" "td-diff" $ dynText yFtFsDiff

        elClass "td" "td-total-points" . dynText
            $ zipDynWith showTaskPointsRounded tp (total <$> xB)
        elClass "td" "as td-norm td-total-points" $ dynText yAsScore
        elClass "td" "td-diff" $ dynText yFtAsDiff

        elClass "td" "as td-norm td-total-points" $ dynText yAsScore
        elClass "td" "fs td-norm td-total-points" $ dynText yFsScore
        elClass "td" "td-diff" $ dynText zAsFsDiff

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
    let dnfMinor =
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
        elClass "td" "as td-norm td-placing" $ text ""
        elClass "td" "fs td-norm td-placing" $ text ""
        elClass "td" "ft td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        elClass "td" "td-total-points" $ text "0"
        dnfMinor
        return ()
