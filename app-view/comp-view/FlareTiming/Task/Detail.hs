module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, intercalate, pack)

import WireTypes.ZoneKind (Shape(..))
import WireTypes.Pilot (nullPilot)
import WireTypes.Comp
    ( Comp(..), Task(..)
    , getRaceRawZones, getStartGates, getOpenShape, getSpeedSection
    )
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    ( getTaskScore, getTaskValidityWorking, getTaskLengthSphericalEdge
    , getTaskPilotDnf, getTaskPilotNyp, getTaskPilotTrack, emptyRoute
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

raceNote :: T.Text
raceNote = "* A clock for each start gate"

elapsedNote :: T.Text
elapsedNote = "* Each pilot on their own clock"

openNote :: Maybe Shape -> T.Text
openNote (Just Vector{}) = " open distance on a heading"
openNote (Just Star{}) = " open distance"
openNote _ = ""

taskTileZones
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLength)
    -> m ()
taskTileZones t len = do
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate " - " <$> zs
    let ss = getSpeedSection <$> t
    let gs = length . getStartGates <$> t
    let d = ffor len (maybe "" $ \TaskLength{..} ->
                showTaskDistance taskRoute)

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
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
                            elClass "span" "level-item level-right has-text-info" $
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
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail ix@(IxTask _) cs task vy a = do
    let utc = utcOffset . head <$> cs
    pb <- getPostBuild
    s <- holdDyn [] =<< getTaskScore ix pb
    vw <- holdDyn Nothing =<< getTaskValidityWorking ix pb
    nyp <- holdDyn [] =<< getTaskPilotNyp ix pb
    dnf <- holdDyn [] =<< getTaskPilotDnf ix pb
    let lnTask = holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb
    ln <- (fmap . fmap) taskLength lnTask
    legs <- (fmap . fmap) taskLegs lnTask
    routes <- holdDyn emptyRoute =<< getTaskLengthSphericalEdge ix pb

    let ps = (fmap . fmap) points a
    let tp = (fmap . fmap) taskPoints a
    let wg = (fmap . fmap) weight a

    taskTileZones task ln
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

                TaskTabAbsent -> tableAbsent nyp dnf task
                TaskTabValidity -> viewValidity vy vw
                TaskTabScore -> tableScore utc ln dnf vy wg ps tp s)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ = return never
