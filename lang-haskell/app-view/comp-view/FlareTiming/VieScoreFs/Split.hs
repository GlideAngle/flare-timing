module FlareTiming.VieScoreFs.Split (tableVieScoreFsSplit) where

import Prelude hiding (min)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
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
    , showTaskPointsRounded
    , showTaskPointsDiff
    , showRounded
    )
import WireTypes.ValidityWorking (ValidityWorking(..), TimeValidityWorking(..))
import WireTypes.Comp (UtcOffset(..), Discipline(..), MinimumDistance(..))
import WireTypes.Pilot (Pilot(..), Dnf(..), DfNoTrack(..), pilotIdsWidth)
import qualified WireTypes.Pilot as Pilot (DfNoTrackPilot(..))
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Score.Show
import FlareTiming.Comms (AltDot)

tableVieScoreFsSplit
    :: MonadWidget t m
    => AltDot
    -> Dynamic t UtcOffset
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
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> m ()
tableVieScoreFsSplit altDot utcOffset hgOrPg free sgs _ln dnf' dfNt _vy vw _wg pt tp sDfs sAltFs = do
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
                elAttr "th" ("colspan" =: "2" <> "class" =: "is-light") $ text "Place"
                el "th" $ text ""

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-distance-points")
                    $ text "Distance"

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-time-points")
                    $ text "Time"

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-leading-points")
                    $ text "Lead"

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-arrival-points")
                    $ text "Arrival"

                elAttr "th" ("colspan" =: "3" <> "class" =: "th-total-points")
                    $ text "Total"

            el "tr" $ do
                let altName = T.pack $ show altDot

                elClass "th" "th-norm th-placing" $ text "Ft"
                elClass "th" "th-placing" $ text altName
                elClass "th" "th-pilot" . dynText $ ffor w hashIdHyphenPilot

                elClass "th" "th-distance-points" $ text "Ft"
                elClass "th" "th-norm th-distance-points" $ text altName
                elClass "th" "th-norm th-diff" $ text "Δ"

                elDynClass "th" (fst <$> cTimePoints) $ text "Ft"
                elClass "th" "th-norm th-time-points" $ text altName
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-leading-points" $ text "Ft"
                elClass "th" "th-norm th-leading-points" $ text "Ft"
                elClass "th" "th-norm th-diff" $ text "Δ"

                elDynClass "th" (fst <$> cArrivalPoints) $ text "Ft"
                elClass "th" "th-norm th-arrival-points" $ text altName
                elClass "th" "th-norm th-diff" $ text "Δ"

                elClass "th" "th-total-points" $ text "Ft"
                elClass "th" "th-norm th-total-points" $ text altName
                elClass "th" "th-norm th-diff" $ text "Δ"

            elClass "tr" "tr-allocation" $ do
                elAttr "th" ("colspan" =: "3" <> "class" =: "th-allocation") $ text "Available Points"

                elClass "th" "th-distance-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskDistancePoints (Just x) x)
                        . Pt.distance
                        )
                    <$> pt

                thSpace
                thSpace

                elClass "th" "th-time-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskTimePoints (Just x) x)
                        . Pt.time
                        )
                    <$> pt

                thSpace
                thSpace

                elClass "th" "th-leading-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskLeadingPoints (Just x) x)
                        . Pt.leading
                        )
                    <$> pt

                thSpace
                thSpace

                elClass "th" "th-arrival-alloc" . dynText $
                    maybe
                        ""
                        ( (\x -> showTaskArrivalPoints (Just x) x)
                        . Pt.arrival
                        )
                    <$> pt

                thSpace
                thSpace

                elClass "th" "th-task-alloc" . dynText $
                    maybe
                        ""
                        (\x -> showTaskPointsRounded (Just x) x)
                    <$> tp

                thSpace
                thSpace

        _ <- el "tbody" $ do
            _ <-
                simpleList
                    sDfs
                    (pointRow
                        w
                        (snd <$> cTimePoints)
                        (snd <$> cArrivalPoints)
                        utcOffset
                        free
                        dfNt
                        pt
                        tp
                        (Map.fromList <$> sAltFs))

            dnfRows w dnfPlacing dnf'
            return ()

        let tdFoot = elAttr "td" ("colspan" =: "21")
        let foot = el "tr" . tdFoot . text

        el "tfoot" $ do
            foot "* Any points so annotated are the maximum attainable."
            foot "Δ A difference between total points, mean ± standard deviation."
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
    -> Dynamic t T.Text
    -> Dynamic t T.Text
    -> Dynamic t UtcOffset
    -> Dynamic t MinimumDistance
    -> Dynamic t DfNoTrack
    -> Dynamic t (Maybe Pt.Points)
    -> Dynamic t (Maybe TaskPoints)
    -> Dynamic t (Map.Map Pilot Alt.AltBreakdown)
    -> Dynamic t (Pilot, Breakdown)
    -> m ()
pointRow w cTime cArrival _utcOffset _free dfNt pt tp sAltFs x = do
    let pilot = fst <$> x
    let xB = snd <$> x

    (yRank, yScore, yDiff, yDistance, yDistanceDiff, yLeading, yLeadingDiff, yArrival, yArrivalDiff, yTime, yTimeDiff) <- sample . current
                $ ffor3 pilot sAltFs x (\pilot' sAltFs' (_, Breakdown{total = p', breakdown = Points{distance = d', leading = l', arrival = a', time = t'}}) ->
                case Map.lookup pilot' sAltFs' of
                    Nothing -> ("", "", "", "", "", "", "", "", "", "", "")
                    Just
                        Alt.AltBreakdown
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

    let classPilot = ffor3 w pilot dfNt (\w' p (DfNoTrack ps) ->
                        let n = showPilot w' p in
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
            $ zipDynWith showTaskPointsRounded tp (total <$> xB)

        elClass "td" "td-norm td-total-points" . text $ yScore
        elClass "td" "td-norm td-total-points" . text $ yDiff

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
        elClass "td" "td-pilot" . dynText $ ffor2 w pilot showPilot
        dnfMajor
        elClass "td" "td-total-points" $ text "0"
        dnfMinor
        return ()
