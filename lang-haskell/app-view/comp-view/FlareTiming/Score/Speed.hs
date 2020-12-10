module FlareTiming.Score.Speed (tableScoreSpeed) where

import Prelude hiding (min)
import Reflex.Dom
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Data.Map.Strict as Map

import WireTypes.Route (TaskLength(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import qualified WireTypes.Point as Pt (Points(..))
import qualified WireTypes.Point as Wg (Weights(..))
import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.Point
    ( TaskPlacing(..), TaskPoints(..), Breakdown(..), Velocity(..), StartGate(..)
    , cmpTime
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Time (timeZone, showT, showTDiff)
import FlareTiming.Score.Show

tableScoreSpeed
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Discipline
    -> Dynamic t MinimumDistance
    -> Dynamic t [StartGate]
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
    -> m ()
tableScoreSpeed utcOffset hgOrPg _free sgs ln dnf' dfNt _vy vw _wg _pt _tp sDfs sAltFs = do
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

    _ <- elDynClass "table" tableClass $ do
        el "thead" $ do

            el "tr" $ do
                elAttr "th" ("colspan" =: "2") $ text ""
                elAttr "th" ("colspan" =: "11" <> "class" =: "th-speed-section") . dynText
                    $ showSpeedSection <$> ln

                elClass "th" "th-speed" $ text "Speed"

            el "tr" $ do
                elClass "th" "th-placing" $ text "Place"
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot
                elClass "th" "th-start-start" $ text "Start"

                elClass "th" "th-norm th-start" . dynText
                    $ ffor sgs (\case [] -> "✓-Start"; _ -> "✓-Gate")

                elClass "th" "th-norm th-time-diff" . dynText
                    $ ffor sgs (\case [] -> "Δ-Start"; _ -> "Δ-Gate")

                elClass "th" "th-start-gate" $ text "Gate"

                elClass "th" "th-time-end" $ text "End"
                elClass "th" "th-norm th-time-end" $ text "✓-End"
                elClass "th" "th-norm th-time-diff" $ text "Δ-End"

                elClass "th" "th-time" $ text "Time †"

                elClass "th" "th-norm th-time" . dynText
                    $ ffor sgs (\case [] -> "✓-Pace"; _ -> "✓-Time")

                elClass "th" "th-norm th-time-diff" $ dynText
                    $ ffor sgs (\case [] -> "Δ-Pace"; _ -> "Δ-Time")

                elClass "th" "th-pace" $ text "Pace ‡"
                elClass "th" "th-speed-units" $ text "(km/h)"

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    (sortBy cmpTime <$> sDfs)
                    (pointRow
                        w
                        utcOffset
                        dfNt
                        (Map.fromList <$> sAltFs))

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "16")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "† \"Time\" is the time across the speed section from time zero of the start gate taken."
            foot "‡ \"Pace\" is the time across the speed section from the time of crossing the start for the last time."
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
    -> Dynamic t DfNoTrack
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w utcOffset dfNt sAltFs x = do
    let tz = timeZone <$> utcOffset
    tz' <- sample . current $ timeZone <$> utcOffset
    let pilot = fst <$> x
    let xB = snd <$> x
    let v = velocity . snd <$> x

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
                        if p `elem` (Pilot.pilot <$> ps)
                           then ("pilot-dfnt", n <> " ☞ ")
                           else ("", n))

    (ySs, ySsDiff, yEs, yEsDiff, yEl, yElDiff) <- sample . current
                $ ffor3 pilot sAltFs x (\pilot' sAltFs' (_, Breakdown{velocity = v'}) ->
                case (v', Map.lookup pilot' sAltFs') of
                    (Just Velocity{ss, gs, es, gsElapsed = gsElap, ssElapsed = ssElap}, Just Alt.AltBreakdown {ss = ss', es = es', timeElapsed = elap'}) ->
                        let (start, elap) =
                                case (ss, gs) of
                                    (_, Just (StartGate g)) -> (Just g, gsElap)
                                    (Just _, _) -> (ss, ssElap)
                                    _ -> (Nothing, Nothing)
                        in
                            ( maybe "" (showT tz') ss'
                            , fromMaybe "" (showTDiff <$> ss' <*> start)
                            , maybe "" (showT tz') es'
                            , fromMaybe "" (showTDiff <$> es' <*> es)
                            , maybe "" showPilotTime elap'
                            , fromMaybe "" (showPilotTimeDiff <$> elap' <*> elap)
                            )

                    _ -> ("", "", "", "", "", ""))

    elDynClass "tr" (fst <$> classPilot) $ do
        elClass "td" "td-placing" . dynText $ showRank . place <$> xB
        elClass "td" "td-pilot" . dynText $ snd <$> classPilot

        elClass "td" "td-start-start" . dynText $ (maybe "" . showSs) <$> tz <*> v
        elClass "td" "td-norm td-start-gate" . text $ ySs
        elClass "td" "td-norm td-time-diff" . text $ ySsDiff

        elClass "td" "td-start-gate" . dynText $ (maybe "" . showGs) <$> tz <*> v

        elClass "td" "td-time-end" . dynText $ (maybe "" . showEs) <$> tz <*> v
        elClass "td" "td-norm td-time-end" . text $ yEs
        elClass "td" "td-norm td-time-diff" . text $ yEsDiff

        elClass "td" "td-time" . dynText $ maybe "" showGsVelocityTime <$> v
        elClass "td" "td-norm td-time" . text $ yEl
        elClass "td" "td-norm td-time-diff" . text $ yElDiff

        elClass "td" "td-pace" . dynText $ maybe "" showSsVelocityTime <$> v
        elClass "td" "td-speed" . dynText $ maybe "" showVelocityVelocity <$> v

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
                        <> "colspan" =: "12"
                        <> "class" =: "td-dnf"
                        )
                        $ text "DNF"
                    return ()

    elClass "tr" "tr-dnf" $ do
        elClass "td" "td-placing" . text $ showRank place
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        dnfMega
        return ()
