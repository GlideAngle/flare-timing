module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, intercalate, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.ZoneKind (Shape(..))
import WireTypes.Pilot (Nyp(..), Dnf(..), DfNoTrack(..), nullPilot)
import WireTypes.Comp
    ( UtcOffset(..), Nominal(..), Comp(..), Task(..), TaskStop(..)
    , getRaceRawZones, getStartGates, getOpenShape, getSpeedSection
    )
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    ( getTaskScore, getTaskValidityWorking, getTaskLengthSphericalEdge
    , getTaskPilotDnf, getTaskPilotNyp, getTaskPilotDfNoTrack
    , getTaskPilotTrack, emptyRoute
    )
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map.View (viewMap)
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

taskTileZones
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Task
    -> Dynamic t (Maybe TaskLength)
    -> m ()
taskTileZones utcOffset t len = do
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

                    dyn_ $ ffor stp (\case
                        Nothing -> return ()
                        Just x -> do
                            elClass "span" "level-item level-right" $
                                el "strong" $ do
                                    text "Stop "
                                    text $ showStop tz x)

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

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t [Comp]
    -> Dynamic t [Nominal]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail ix@(IxTask _) cs ns task vy a = do
    let utc = utcOffset . head <$> cs
    let hgOrPg = discipline . head <$> cs
    let free' = free . head <$> ns
    let sgs = startGates <$> task
    pb <- getPostBuild
    sDf <- holdDyn [] =<< getTaskScore ix pb
    vw <- holdDyn Nothing =<< getTaskValidityWorking ix pb
    nyp <- holdDyn (Nyp []) =<< getTaskPilotNyp ix pb
    dnf <- holdDyn (Dnf []) =<< getTaskPilotDnf ix pb
    dfNt <- holdDyn (DfNoTrack []) =<< getTaskPilotDfNoTrack ix pb
    routes <- holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb
    let ln = taskLength <$> routes
    let legs = taskLegs <$> routes

    let ps = (fmap . fmap) points a
    let tp = (fmap . fmap) taskPoints a
    let wg = (fmap . fmap) weight a

    taskTileZones utc task ln
    es <- simpleList cs (crumbTask task)
    tab <- tabsTask

    _ <- widgetHold (tableTask utc task legs) $
            (\case
                TaskTabGeo -> tableGeo ix

                TaskTabTask -> tableTask utc task legs

                TaskTabMap -> mdo
                    p <- viewMap ix task routes pt
                    p' <- holdDyn nullPilot p
                    t <- getTaskPilotTrack ix p
                    let pt = attachPromptlyDyn p' t

                    return ()

                TaskTabAbsent -> tableAbsent ix nyp dnf dfNt
                TaskTabValidity -> viewValidity vy vw

                TaskTabScore -> tableScore utc hgOrPg free' sgs ln dnf dfNt vy wg ps tp sDf)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ _ = return never
