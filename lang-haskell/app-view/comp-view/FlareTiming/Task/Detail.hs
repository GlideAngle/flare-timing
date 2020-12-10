module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, intercalate, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.ZoneKind (Shape(..))
import WireTypes.Pilot
    (Pilot(..), Nyp(..), Dnf(..), DfNoTrack(..), Penal(..), nullPilot)
import qualified WireTypes.Comp as Comp
import WireTypes.Comp
    ( Discipline(..), UtcOffset(..), Nominal(..), Comp(..), Task(..), TaskStop(..), ScoreBackTime
    , EarthModel(..), EarthMath(..)
    , getRaceRawZones, getStartGates, getOpenShape, getSpeedSection
    , showScoreBackTime
    )
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
import WireTypes.Cross (TrackFlyingSection(..), TrackScoredSection(..))
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    ( AltDot(..)
    , getTaskScore, getTaskAltScore
    , getTaskBolsterStats, getTaskBonusBolsterStats
    , getTaskReach, getTaskBonusReach
    , getTaskEffort, getTaskLanding, getTaskAltLanding
    , getTaskArrival, getTaskAltArrival, getTaskLead, getTaskTime
    , getTaskValidityWorking, getTaskAltValidityWorking
    , getTaskLengthAltSphere
    , getTaskLengthSphericalEdge
    , getTaskLengthEllipsoidEdge
    , getTaskLengthProjectedEdgeSpherical
    , getTaskPilotDnf, getTaskPilotNyp, getTaskPilotDfNoTrack
    , getTaskPilotTrack
    , getTaskPilotTrackFlyingSection, getTaskFlyingSectionTimes
    , getTaskPilotTrackScoredSection
    , getTaskPilotCross, getTaskPilotTag
    , emptyRoute
    )
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map.View (viewMap)
import FlareTiming.Map.Track (tableTrack)

import FlareTiming.ViePlot.Reach (reachViePlot)
import FlareTiming.ViePlot.Effort (effortViePlot)
import FlareTiming.ViePlot.Time (timeViePlot)
import FlareTiming.ViePlot.LeadCoef (leadCoefViePlot)
import FlareTiming.ViePlot.LeadArea (leadAreaViePlot)
import FlareTiming.ViePlot.Arrival (arrivalViePlot)

import FlareTiming.Plot.Weight (weightPlot)
import FlareTiming.Plot.Reach (reachPlot)
import FlareTiming.Plot.Effort (effortPlot)
import FlareTiming.Plot.Time (timePlot)
import FlareTiming.Plot.LeadCoef (leadCoefPlot)
import FlareTiming.Plot.LeadArea (leadAreaPlot)
import FlareTiming.Plot.Arrival (arrivalPlot)
import FlareTiming.Plot.Valid (validPlot)

import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Nav.TabTask (TaskTab(..), tabsTask)
import FlareTiming.Nav.TabScore (ScoreTab(..), tabsScore)
import FlareTiming.Nav.TabPenal (PenalTab(..), tabsPenal)
import FlareTiming.Nav.TabPlot (PlotTab(..), tabsPlot)
import FlareTiming.Nav.TabPlotLead (PlotLeadTab(..), tabsPlotLead)

import FlareTiming.Nav.TabVie (VieTab(..), tabsVie)
import FlareTiming.Nav.TabVieScoreFs (VieScoreFsTab(..), tabsVieScoreFs)
import FlareTiming.Nav.TabViePlotFs (ViePlotFsTab(..), tabsViePlotFs)
import FlareTiming.Nav.TabViePlotFsLead (ViePlotFsLeadTab(..), tabsViePlotFsLead)

import FlareTiming.Task.VieScoreBoth.Over (tableVieScoreBothOver)
import FlareTiming.Task.VieScoreFs.Over (tableVieScoreFsOver)
import FlareTiming.Task.VieScoreFs.Split (tableVieScoreFsSplit)
import FlareTiming.Task.VieScoreFs.Reach (tableVieScoreFsReach)
import FlareTiming.Task.VieScoreFs.Effort (tableVieScoreFsEffort)
import FlareTiming.Task.VieScoreFs.Speed (tableVieScoreFsSpeed)
import FlareTiming.Task.VieScoreFs.Time (tableVieScoreFsTime)
import FlareTiming.Task.VieScoreFs.Arrive (tableVieScoreFsArrive)

import FlareTiming.Task.Score.Over (tableScoreOver)
import FlareTiming.Task.Score.Split (tableScoreSplit)
import FlareTiming.Task.Score.Reach (tableScoreReach)
import FlareTiming.Task.Score.Effort (tableScoreEffort)
import FlareTiming.Task.Score.Speed (tableScoreSpeed)
import FlareTiming.Task.Score.Time (tableScoreTime)
import FlareTiming.Task.Score.Arrive (tableScoreArrive)

import FlareTiming.Task.Penal.Jump (tablePenalJump)
import FlareTiming.Task.Penal.EssGoal (tablePenalEssGoal)
import FlareTiming.Task.Penal.Manual (tablePenalManual)

import FlareTiming.Nav.TabBasis (BasisTab(..), tabsBasis)
import FlareTiming.Task.Geo (tableGeo)
import FlareTiming.Task.Turnpoints (tableTask)
import FlareTiming.Task.Absent (tableAbsent)
import FlareTiming.Task.Validity (viewValidity)
import FlareTiming.Time (showT, timeZone)

raceNote :: T.Text
raceNote = "* A clock for each start gate"

elapsedNote :: T.Text
elapsedNote = "* Each pilot on their own clock"

openNote :: Maybe Shape -> T.Text
openNote (Just Vector{}) = " open distance on a heading"
openNote (Just Star{}) = " open distance"
openNote _ = ""

showStop :: TimeZone -> TaskStop -> T.Text
showStop tz TaskStop{..} = showT tz announced

showRetro :: TimeZone -> TaskStop -> T.Text
showRetro tz TaskStop{..} = showT tz retroactive

taskTileZones
    :: MonadWidget t m
    => Dynamic t Discipline
    -> Dynamic t UtcOffset
    -> Dynamic t (Maybe ScoreBackTime)
    -> Dynamic t Task
    -> Dynamic t (Maybe TaskLength)
    -> m ()
taskTileZones hgOrPg utcOffset sb t len = do
    tz <- sample . current $ timeZone <$> utcOffset
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate "-" <$> zs
    let ss = getSpeedSection <$> t
    let gsNum = length . getStartGates <$> t
    let d = ffor len (maybe "" $ \TaskLength{..} -> showTaskDistance taskRoute)
    let stp = stopped <$> t

    let boxClass =
            ffor stp (\x ->
                let c = "tile is-child box" in
                c <> maybe "" (const " notification is-danger") x)

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elDynClass "div" boxClass $ do
                elAttr "p" ("class" =: "level title is-3" <> "style" =: "overflow: hidden") $ do
                    elClass "span" "level-item level-left" $ do
                        dynText title

                dyn_ $ ffor ss (\case
                    Just _ -> do
                        let kind = ffor gsNum (\case
                                    0 -> " elapsed time*"
                                    1 -> " race to goal* with a single start gate"
                                    n -> " race to goal* with " <> (T.pack . show $ n) <> " start gates")

                        let sideNote = ffor gsNum (\case 0 -> elapsedNote; _ -> raceNote)

                        elClass "p" "level subtitle is-6" $ do
                            elClass "span" "level-item level-left" $ do
                                dynText d
                                dynText $ kind

                            elClass "span" "level-item level-right" $
                                dynText $ sideNote

                    Nothing -> do
                        let openKind = openNote . getOpenShape <$> t

                        elClass "p" "level subtitle is-6" $ do
                            elClass "span" "level-item level-left" $ do
                                dynText d
                                dynText $ openKind)

                dyn_ $ ffor3 hgOrPg stp sb (\hgOrPg' x y ->
                    case (hgOrPg', x) of
                        (_, Nothing) -> return ()
                        (Paragliding, Just x') -> do
                            let stop = "Stopped at " <> showStop tz x' <> "†"

                            elClass "p" "level subtitle is-6" $ do
                                elClass "span" "level-item level-left" $ do
                                            elClass "span" "level-item level-right" $
                                                el "strong" $ text stop

                                case y of
                                    Nothing -> return ()
                                    Just y' -> do
                                        let back =
                                                "† Scored back by "
                                                <> (T.pack . showScoreBackTime $ y')
                                                <> " to "
                                                <> showRetro tz x'

                                        elClass "span" "level-item level-right" $
                                            el "strong" $ text back
                        (HangGliding, Just x') -> do
                            let stop = "Stopped at " <> showStop tz x' <> "†"
                            sbGates <- sample . current $ ffor gsNum (\case
                                            0 -> "15 mins with no start gates"
                                            1 -> "15 mins with one start gate"
                                            _ -> "the start gate interval")

                            elClass "p" "level subtitle is-6" $ do
                                elClass "span" "level-item level-left" $ do
                                            elClass "span" "level-item level-right" $
                                                el "strong" $ text stop

                                let back =
                                        "† Scored back by "
                                        <> sbGates
                                        <> " to "
                                        <> showRetro tz x'

                                elClass "span" "level-item level-right" $
                                    el "strong" $ text back)

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t Comp
    -> Dynamic t Nominal
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail ix@(IxTask _) comp nom task vy vyAlt alloc = do
    let utc = utcOffset <$> comp
    let sb = scoreBack <$> comp
    let hgOrPg = discipline <$> comp
    let free' = free <$> nom
    let sgs = startGates <$> task
    let penalAuto = Penal . Comp.penalsAuto <$> task
    let penal = Penal . Comp.penals <$> task
    let tweak = Comp.taskTweak <$> task
    let early = earlyStart <$> task
    let stp = stopped <$> task
    pb <- getPostBuild
    sDf <- holdDyn [] =<< getTaskScore ix pb
    sAltFs <- holdDyn [] =<< getTaskAltScore AltFs ix pb
    sAltAs <- holdDyn [] =<< getTaskAltScore AltAs ix pb
    reachStats <- holdDyn Nothing =<< (fmap Just <$> getTaskBolsterStats ix pb)
    bonusStats <- holdDyn Nothing =<< (fmap Just <$> getTaskBonusBolsterStats ix pb)
    reach <- holdDyn Nothing =<< (fmap Just <$> getTaskReach ix pb)
    bonusReach <- holdDyn Nothing =<< (fmap Just <$> getTaskBonusReach ix pb)
    ef <- holdDyn Nothing =<< (fmap Just <$> getTaskEffort ix pb)
    lg <- holdDyn Nothing =<< (getTaskLanding ix pb)
    lgN <- holdDyn Nothing =<< (getTaskAltLanding ix pb)
    av <- holdDyn Nothing =<< (fmap Just <$> getTaskArrival ix pb)
    avN <- holdDyn Nothing =<< (fmap Just <$> getTaskAltArrival ix pb)
    ld <- holdDyn Nothing =<< (fmap Just <$> getTaskLead ix pb)
    sd <- holdDyn Nothing =<< (fmap Just <$> getTaskTime ix pb)
    ft <- holdDyn Nothing =<< (fmap Just <$> getTaskFlyingSectionTimes ix pb)
    vw <- holdDyn Nothing =<< getTaskValidityWorking ix pb
    vwAlt <- holdDyn Nothing =<< getTaskAltValidityWorking ix pb
    nyp <- holdDyn (Nyp []) =<< getTaskPilotNyp ix pb
    dnf <- holdDyn (Dnf []) =<< getTaskPilotDnf ix pb
    dfNt <- holdDyn (DfNoTrack []) =<< getTaskPilotDfNoTrack ix pb

    sphericalRoutes <- holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb
    ellipsoidRoutes <- holdDyn emptyRoute =<< getTaskLengthEllipsoidEdge ix pb
    let earthMathRoutes = ffor3 comp sphericalRoutes ellipsoidRoutes (\Comp{..} s e ->
            case (earth, earthMath) of
                (EarthAsSphere{}, Haversines) -> s
                (EarthAsEllipsoid{}, Vincenty) -> e
                (EarthAsEllipsoid{}, AndoyerLambert) -> e
                (EarthAsEllipsoid{}, ForsytheAndoyerLambert) -> e
                (EarthAsEllipsoid{}, FsAndoyer) -> e
                _ -> s)

    planarRoute <- holdDyn Nothing =<< getTaskLengthProjectedEdgeSpherical ix pb
    normRoute <- holdDyn Nothing =<< getTaskLengthAltSphere ix pb

    let ln = taskLength <$> earthMathRoutes
    let legs = taskLegs <$> earthMathRoutes

    let ps = (fmap . fmap) points alloc
    let tp = (fmap . fmap) taskPoints alloc
    let wg = (fmap . fmap) weight alloc

    taskTileZones hgOrPg utc sb task ln
    es <- crumbTask ix task comp
    tabTask <- tabsTask

    let taskTabScoreContent = do
            tabScore <- tabsScore
            let tableScoreHold =
                    elAttr "div" ("id" =: "score-overview") $
                        tableScoreOver utc hgOrPg early free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs sAltAs
            _ <- widgetHold tableScoreHold $
                    (\case
                        ScoreTabOver ->
                            tableScoreHold

                        ScoreTabSplit ->
                            elAttr "div" ("id" =: "score-points") $
                                tableScoreSplit utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                        ScoreTabReach ->
                            elAttr "div" ("id" =: "score-reach") $
                                tableScoreReach utc hgOrPg free' sgs ln stp dnf dfNt vw ps sDf sAltFs
                        ScoreTabEffort ->
                            elAttr "div" ("id" =: "score-effort") $
                                tableScoreEffort utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs lg lgN
                        ScoreTabSpeed ->
                            elAttr "div" ("id" =: "score-speed") $
                                tableScoreSpeed utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                        ScoreTabTime ->
                            elAttr "div" ("id" =: "score-time") $
                                tableScoreTime utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                        ScoreTabArrive ->
                            elAttr "div" ("id" =: "score-arrival") $
                                tableScoreArrive utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs)

                    <$> tabScore

            return ()

    _ <- widgetHold taskTabScoreContent $
            (\case
                TaskTabTask -> tableTask utc task legs

                TaskTabMap -> mdo
                    p <- viewMap utc ix task sphericalRoutes ellipsoidRoutes planarRoute normRoute pt
                    _ <- tableTrack utc ptfs''
                    p' <- holdDyn nullPilot p

                    tfs <- getTaskPilotTrackFlyingSection ix p
                    tfs' <- holdDyn (nullPilot, Nothing) (attachPromptlyDyn p' tfs)

                    tss <- getTaskPilotTrackScoredSection ix p
                    tss' <- holdDyn (nullPilot, Nothing) (attachPromptlyDyn p' tss)

                    tts <- holdUniqDyn $ zipDynWith (,) tfs' tss'
                    ptfs <- holdUniqDyn $ zipDynWith (,) p' tts

                    ptfs' <-
                        foldDyn
                            (\a@(p0, ((p1,_), (p2, _))) b ->
                                if | nullPilot `elem` [p0, p1, p2] -> b
                                   | p0 /= p1 -> b
                                   | p1 /= p2 -> b
                                   | otherwise -> a : b)
                            []
                            (updated ptfs)

                    ptfs'' <- holdUniqDyn ptfs'

                    tag <- getTaskPilotTag ix p
                    tag' <- holdDyn (nullPilot, []) (attachPromptlyDyn p' tag)
                    tag'' <- holdUniqDyn tag'

                    cross <- getTaskPilotCross ix p
                    cross' <- holdDyn (nullPilot, Nothing) (attachPromptlyDyn p' cross)
                    cross'' <- holdUniqDyn cross'

                    trk <- getTaskPilotTrack ix p
                    trk' <- holdDyn (nullPilot, []) (attachPromptlyDyn p' trk)
                    trk'' <- holdUniqDyn trk'

                    tagCross <- holdUniqDyn $ zipDynWith (,) tag'' cross''
                    tt <- holdUniqDyn $ zipDynWith (,) trk'' tagCross

                    let pt =
                            push readyTrack
                            $ attachPromptlyDyn ptfs (updated tt)

                    return ()

                TaskTabScore -> taskTabScoreContent

                TaskTabPlot -> do
                    tabPlot <- tabsPlot
                    let plotSplit = weightPlot hgOrPg tweak vy vw alloc ln
                    _ <- widgetHold (plotSplit) $
                            (\case
                                PlotTabSplit -> plotSplit
                                PlotTabReach -> reachPlot task sAltFs reach bonusReach
                                PlotTabEffort -> effortPlot hgOrPg sAltFs ef
                                PlotTabTime -> timePlot sgs sAltFs sd

                                PlotTabLead -> do
                                    tabPlotLead <- tabsPlotLead
                                    let plotLeadCoef = leadCoefPlot ix tweak sAltFs ld
                                    _ <- widgetHold (plotLeadCoef) $
                                            (\case
                                                PlotLeadTabPoint -> plotLeadCoef
                                                PlotLeadTabArea -> leadAreaPlot ix tweak sAltFs ld
                                            )
                                            <$> tabPlotLead
                                    return ()

                                PlotTabArrive -> arrivalPlot hgOrPg tweak av avN
                                PlotTabValid -> validPlot vy vw
                            )
                            <$> tabPlot

                    return ()

                TaskTabBasis -> do
                    tabBasis <- tabsBasis
                    let basisAbsent = tableAbsent utc early ix ln nyp dnf dfNt penalAuto penal sDf
                    _ <- widgetHold basisAbsent $
                            (\case
                                BasisTabAbsent -> basisAbsent
                                BasisTabValidity ->
                                    viewValidity
                                        utc ln free' task
                                        vy vyAlt
                                        vw vwAlt
                                        reachStats bonusStats
                                        reach bonusReach
                                        ft dfNt sAltFs
                                BasisTabGeo -> tableGeo ix comp)

                            <$> tabBasis

                    return ()

                TaskTabPenal -> do
                    tabPenal <- tabsPenal
                    let penalJump =
                            elAttr "div" ("id" =: "score-penal") $
                                tablePenalJump hgOrPg early sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs penalAuto

                    _ <- widgetHold penalJump $
                            (\case
                                PenalTabJump -> penalJump

                                PenalTabEssGoal ->
                                    elAttr "div" ("id" =: "score-penal") $
                                        tablePenalEssGoal hgOrPg tweak early sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs

                                PenalTabManual ->
                                    elAttr "div" ("id" =: "score-penal") $
                                        tablePenalManual hgOrPg early sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs penal)

                            <$> tabPenal

                    return ()

                TaskTabVie -> do
                    tabVie <- tabsVie
                    let vieHold =
                            elAttr "div" ("id" =: "score-overview") $
                                tableVieScoreBothOver utc hgOrPg early free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs sAltAs

                    _ <- widgetHold vieHold $
                            (\case
                                VieTabScore -> vieHold

                                VieTabScoreFs -> do
                                    tabVieScoreFs <- tabsVieScoreFs
                                    let tableVieScoreFsHold =
                                            elAttr "div" ("id" =: "score-overview") $
                                                tableVieScoreFsOver utc hgOrPg early free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs sAltAs

                                    _ <- widgetHold tableVieScoreFsHold $
                                            (\case
                                                VieScoreFsTabOver ->
                                                    tableVieScoreFsHold

                                                VieScoreFsTabSplit ->
                                                    elAttr "div" ("id" =: "score-points") $
                                                        tableVieScoreFsSplit utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                                                VieScoreFsTabReach ->
                                                    elAttr "div" ("id" =: "score-reach") $
                                                        tableVieScoreFsReach utc hgOrPg free' sgs ln stp dnf dfNt vw ps sDf sAltFs
                                                VieScoreFsTabEffort ->
                                                    elAttr "div" ("id" =: "score-effort") $
                                                        tableVieScoreFsEffort utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs lg lgN
                                                VieScoreFsTabSpeed ->
                                                    elAttr "div" ("id" =: "score-speed") $
                                                        tableVieScoreFsSpeed utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                                                VieScoreFsTabTime ->
                                                    elAttr "div" ("id" =: "score-time") $
                                                        tableVieScoreFsTime utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs
                                                VieScoreFsTabArrive ->
                                                    elAttr "div" ("id" =: "score-arrival") $
                                                        tableVieScoreFsArrive utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf sAltFs)

                                            <$> tabVieScoreFs

                                    return ()

                                VieTabPlotFs -> do
                                    tabViePlotFs <- tabsViePlotFs
                                    let plotReach = reachViePlot task sAltFs reach bonusReach
                                    _ <- widgetHold (plotReach) $
                                            (\case
                                                ViePlotFsTabReach -> plotReach
                                                ViePlotFsTabEffort -> effortViePlot hgOrPg sAltFs ef
                                                ViePlotFsTabTime -> timeViePlot sgs sAltFs sd

                                                ViePlotFsTabLead -> do
                                                    tabViePlotFsLead <- tabsViePlotFsLead
                                                    let plotLeadCoef = leadCoefViePlot ix tweak sAltFs ld
                                                    _ <- widgetHold (plotLeadCoef) $
                                                            (\case
                                                                ViePlotFsLeadTabPoint -> plotLeadCoef
                                                                ViePlotFsLeadTabArea -> leadAreaViePlot ix tweak sAltFs ld
                                                            )
                                                            <$> tabViePlotFsLead
                                                    return ()

                                                ViePlotFsTabArrive -> arrivalViePlot hgOrPg tweak av avN
                                            )
                                            <$> tabViePlotFs

                                    return ()
                            )

                            <$> tabVie

                    return ())

            <$> tabTask

    return es

taskDetail IxTaskNone _ _ _ _ _ _ = return never

readyTrack
    :: Monad m
    =>
        (
            ( Pilot
            ,
                ( (Pilot, Maybe TrackFlyingSection)
                , (Pilot, Maybe TrackScoredSection)
                )
            )
        ,
            ( (Pilot, [a])
            , ((Pilot, [b]), (Pilot, Maybe c))
            )
        )
    -> m
        (Maybe
            (
                ( Pilot
                ,
                    ( (Pilot, Maybe TrackFlyingSection)
                    , (Pilot, Maybe TrackScoredSection)
                    )
                )
            ,
                ( (Pilot, [a])
                , ((Pilot, [b]), (Pilot, Maybe c))
                )
            )
        )
readyTrack x@((p, ((fq, tf), (sq, ts))), ((xq, xs), ((yq, ys), (zq, _))))
    | p == nullPilot = return Nothing
    | fq == nullPilot = return Nothing
    | sq == nullPilot = return Nothing
    | xq == nullPilot = return Nothing
    | yq == nullPilot = return Nothing
    | zq == nullPilot = return Nothing
    | p /= fq || p /= sq || p /= xq || p /= yq || p /= zq = return Nothing
    | null xs = return Nothing
    | null ys = return Nothing
    | otherwise = return $
        case (tf, ts) of
            (Nothing, _) -> Nothing
            (_, Nothing) -> Nothing
            (Just TrackFlyingSection{flyingFixes = Nothing}, _) -> Nothing
            (_, Just TrackScoredSection{scoredFixes = Nothing}) -> Nothing
            (Just _, Just _) -> Just x
