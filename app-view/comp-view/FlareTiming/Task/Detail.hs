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
    ( UtcOffset(..), Nominal(..), Comp(..), Task(..), TaskStop(..), ScoreBackTime
    , getRaceRawZones, getStartGates, getOpenShape, getSpeedSection
    , showScoreBackTime
    )
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
import WireTypes.Cross (TrackFlyingSection(..))
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    ( getTaskScore, getTaskArrival, getTaskLead, getTaskTime
    , getTaskValidityWorking, getTaskLengthSphericalEdge
    , getTaskPilotDnf, getTaskPilotNyp, getTaskPilotDfNoTrack
    , getTaskPilotTrack, getTaskPilotTrackFlyingSection, emptyRoute
    )
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map.View (viewMap)
import FlareTiming.Plot.Weight (weightPlot)
import FlareTiming.Plot.Arrival (arrivalPlot)
import FlareTiming.Plot.Lead (leadPlot)
import FlareTiming.Plot.Time (timePlot)
import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Score (tableScore)
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
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe ScoreBackTime)
    -> Dynamic t Task
    -> Dynamic t (Maybe TaskLength)
    -> m ()
taskTileZones utcOffset sb t len = do
    tz <- sample . current $ timeZone <$> utcOffset
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate " - " <$> zs
    let ss = getSpeedSection <$> t
    let gs = length . getStartGates <$> t
    let d = ffor len (maybe "" $ \TaskLength{..} -> showTaskDistance taskRoute)
    let stp = stopped <$> t

    let boxClass =
            ffor stp (\x ->
                let c = "tile is-child box" in
                c <> maybe "" (const " notification is-danger") x)

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elDynClass "div" boxClass $ do
                elClass "p" "level title is-3" $ do
                    elClass "span" "level-item level-left" $ do
                        dynText title

                dyn_ $ ffor ss (\case
                    Just _ -> do
                        let kind = ffor gs (\case
                                    0 -> " elapsed time*"
                                    1 -> " race to goal* with a single start gate"
                                    n -> " race to goal* with " <> (T.pack . show $ n) <> " start gates")

                        let sideNote = ffor gs (\case 0 -> elapsedNote; _ -> raceNote)

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

                dyn_ $ ffor2 stp sb (\x y ->
                    case x of
                        Nothing -> return ()
                        Just x' -> do
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
                                            el "strong" $ text back)

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t [Comp]
    -> Dynamic t [Nominal]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail ix@(IxTask _) cs ns task vy alloc = do
    let utc = utcOffset . head <$> cs
    let sb = scoreBack . head <$> cs
    let hgOrPg = discipline . head <$> cs
    let free' = free . head <$> ns
    let sgs = startGates <$> task
    let penal = Penal . Comp.penals <$> task
    let tweak = Comp.taskTweak <$> task
    pb <- getPostBuild
    sDf <- holdDyn [] =<< getTaskScore ix pb
    av <- holdDyn Nothing =<< (fmap Just <$> getTaskArrival ix pb)
    ld <- holdDyn Nothing =<< (fmap Just <$> getTaskLead ix pb)
    sd <- holdDyn Nothing =<< (fmap Just <$> getTaskTime ix pb)
    vw <- holdDyn Nothing =<< getTaskValidityWorking ix pb
    nyp <- holdDyn (Nyp []) =<< getTaskPilotNyp ix pb
    dnf <- holdDyn (Dnf []) =<< getTaskPilotDnf ix pb
    dfNt <- holdDyn (DfNoTrack []) =<< getTaskPilotDfNoTrack ix pb
    routes <- holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb
    let ln = taskLength <$> routes
    let legs = taskLegs <$> routes

    let ps = (fmap . fmap) points alloc
    let tp = (fmap . fmap) taskPoints alloc
    let wg = (fmap . fmap) weight alloc

    taskTileZones utc sb task ln
    es <- simpleList cs (crumbTask task)
    tab <- tabsTask

    _ <- widgetHold (weightPlot hgOrPg tweak alloc) $
            (\case
                TaskTabGeo ->
                    tableGeo ix

                TaskTabTask ->
                    tableTask utc task legs

                TaskTabMap -> mdo
                    p <- viewMap ix task routes pt
                    p' <- holdDyn nullPilot p

                    tfs <- getTaskPilotTrackFlyingSection ix p
                    tfs' <- holdDyn Nothing tfs

                    trk <- getTaskPilotTrack ix p
                    trk' <- holdDyn [] trk
                    trk'' <- holdUniqDyn trk'

                    ptfs <- holdUniqDyn $ zipDynWith (,) p' tfs'

                    let pt =
                            push readyTrack
                            $ attachPromptlyDyn ptfs (updated trk'')

                    return ()

                TaskTabAbsent ->
                    tableAbsent utc ix ln nyp dnf dfNt penal

                TaskTabValidity ->
                    viewValidity vy vw

                TaskTabScore ->
                    tableScore utc hgOrPg free' sgs ln dnf dfNt vy vw wg ps tp sDf

                TaskTabSplit ->
                    weightPlot hgOrPg tweak alloc

                TaskTabArrive ->
                    arrivalPlot hgOrPg av

                TaskTabLead ->
                    leadPlot hgOrPg ld

                TaskTabTime  ->
                    timePlot hgOrPg sd)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ _ = return never

readyTrack
    :: Monad m
    => ((Pilot, Maybe TrackFlyingSection), [a])
    -> m (Maybe ((Pilot, Maybe TrackFlyingSection), [a]))
readyTrack x@((p, t), xs)
    | p == nullPilot = return Nothing
    | null xs = return Nothing
    | otherwise = return $
        case t of
            Nothing -> Nothing
            Just TrackFlyingSection{flyingFixes = Nothing} -> Nothing
            Just TrackFlyingSection{scoredFixes = Nothing} -> Nothing
            Just _ -> Just x
