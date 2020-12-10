module FlareTiming.Score.Effort (tableScoreEffort) where

import Prelude hiding (map)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..), StartGate(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import qualified WireTypes.Point as Bk (Breakdown(..))
import WireTypes.Point
    ( TaskPlacing(..), TaskPoints(..), PilotDistance(..), ReachToggle(..)
    , showPilotDistance, showTaskDifficultyPoints, cmpEffort
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import WireTypes.Effort
    ( TaskLanding(..), Lookahead(..)
    , Chunking(..), IxChunk(..), Chunk(..)
    , ChunkDifficulty(..), SumOfDifficulty(..)
    )
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Score.Show

tableScoreEffort
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
    -> Dynamic t [(Pilot, Bk.Breakdown)]
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> Dynamic t (Maybe TaskLanding)
    -> Dynamic t (Maybe TaskLanding)
    -> m ()
tableScoreEffort utcOffset hgOrPg free sgs ln dnf' dfNt _vy vw _wg pt _tp sDfs _sAltFs lg' _lgN' = do
    let w = ffor sDfs (pilotIdsWidth . fmap fst)
    let dnf = unDnf <$> dnf'
    lenDnf :: Int <- sample . current $ length <$> dnf
    lenDfs :: Int <- sample . current $ length <$> sDfs
    let dnfPlacing =
            (if lenDnf == 1 then TaskPlacing else TaskPlacingEqual)
            . fromIntegral
            $ lenDfs + 1

    let describeChunking = \case
            Just
                TaskLanding
                    { landout
                    , lookahead = Just (Lookahead n)
                    , chunking =
                        Just
                            Chunking
                                { sumOf = SumOfDifficulty diff
                                , endChunk = (IxChunk ixN, Chunk (PilotDistance ec))
                                }
                    } ->
                        (T.pack $ printf "%d landouts over " landout)
                        <> (showPilotDistance 1 (PilotDistance $ ec + 0.1)) <> " km"
                        <> (T.pack $ printf " or %d chunks" ixN)
                        <> " sum to "
                        <> T.pack (show diff)
                        <> (T.pack $ printf " looking ahead %.1f km" (0.1 * fromIntegral n :: Double))
            _ -> ""

    let msgChunking = ffor lg' describeChunking

    let pilotChunk = \case
            Just TaskLanding{difficulty = Just ds} ->
                concat
                [ (\p -> (p, chunk)) <$> downers
                | ChunkDifficulty{chunk, downers} <- ds
                ]
            _ -> []

    let pChunks = ffor lg' pilotChunk

    let tableClass =
            let tc = "table is-striped is-narrow is-fullwidth" in
            ffor2 hgOrPg sgs (\x gs ->
                let y = T.pack . show $ x in
                y <> (if null gs then " " else " sg ") <> tc)

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "7") $ dynText msgChunking

            el "tr" $ do
                elAttr "th" ("colspan" =: "4") $ text ""
                elClass "th" "th-distance-points-breakdown" $ text "Points for Effort (Descending)"

            el "tr" $ do
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-landed-distance" $ text "Landed"
                elClass "th" "th-chunk" $ text "Chunk"
                elClass "th" "th-effort-points" $ text "Effort †"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "2" <> "class" =: "th-allocation") $ text "Available Points (Units)"
                elClass "th" "th-landed-distance-units" $ text "(km)"
                elClass "th" "th-chunk-units" $ text "(km)"

                elClass "th" "th-effort-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDifficultyPoints (Just x) x)
                        . Pt.effort
                        )
                    <$> pt

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    (sortBy cmpEffort <$> sDfs)
                    (pointRow
                        w
                        utcOffset
                        free
                        ln
                        dfNt
                        pt
                        (Map.fromList <$> pChunks)
                    )

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "15")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "† Points awarded for effort are also called distance difficulty points."
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
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Map.Map Pilot IxChunk)
    -> Dynamic t (Pilot, Bk.Breakdown)
    -> m ()
pointRow w _utcOffset free _ln dfNt pt ixChunkMap x = do
    MinimumDistance free' <- sample . current $ free

    let pilot = fst <$> x
    let xB = snd <$> x

    let extraReach = fmap extra . Bk.reach <$> xB
    let points = Bk.breakdown <$> xB

    (classPilot, idNamePilot) <- sample . current $
            ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                let sp = showPilot w' p
                in
                    if p `elem` (Pilot.pilot <$> ps)
                       then ("pilot-dfnt", sp <> " ☞ ")
                       else ("", sp))

    let awardFree = ffor extraReach (\pd ->
            let c = "td-landed-distance" in
            maybe
                (c, "")
                (\(PilotDistance r) ->
                    if r >= free' then (c, "") else
                    (c <> " award-free", T.pack $ printf "%.1f" free'))
                pd)

    landed <- sample . current
                $ ffor x (\(_, Bk.Breakdown{landedMade}) ->
                    maybe "" (showPilotDistance 3) landedMade)

    let pilotChunk pilot' ixChunkMap' = do
            ic@(IxChunk i) <- Map.lookup pilot' ixChunkMap'
            let pd = PilotDistance (0.1 * fromIntegral i :: Double)
            return $ (ic, pd)

    ixChunk <- sample . current
            $ ffor2 pilot ixChunkMap (\p map -> fromMaybe "" $ do
                    (_, pd) <- pilotChunk p map
                    return $ showPilotDistance 1 pd)

    elClass "tr" classPilot $ do
        elClass "td" "td-placing" . dynText $ showRank . Bk.place <$> xB
        elClass "td" "td-pilot" $ text idNamePilot
        elDynClass "td" (fst <$> awardFree) . text $ landed
        elClass "td" "td-chunk" $ text ixChunk
        elClass "td" "td-effort-points" . dynText
            $ showMax Pt.effort showTaskDifficultyPoints pt points

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
    let dnfMega =
            case rows of
                Nothing -> return ()
                Just n -> do
                    elAttr
                        "td"
                        ( "rowspan" =: (T.pack $ show n)
                        <> "colspan" =: "3"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        dnfMega
        return ()
