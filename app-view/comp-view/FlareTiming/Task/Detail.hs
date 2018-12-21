module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (intercalate)

import WireTypes.Comp (Comp(..), Task(..), getRaceRawZones)
import WireTypes.Route (taskLength, taskLegs, taskOptimalRoute)
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    (getTaskScore, getTaskValidityWorking, getTaskLengthSphericalEdge)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)
import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Score (tableScore)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)
import FlareTiming.Task.Validity (viewValidity)

taskTileZones
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
taskTileZones t = do
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate " - " <$> zs

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail t@(IxTask _) cs task vy a = do
    let utc = utcOffset . head <$> cs
    s <- getTaskScore t
    vw <- getTaskValidityWorking t

    let lnTask = getTaskLengthSphericalEdge t
    route <- (fmap . fmap) taskOptimalRoute lnTask
    ln <- (fmap . fmap) taskLength lnTask
    legs <- (fmap . fmap) taskLegs lnTask

    let ps = (fmap . fmap) points a
    let tp = (fmap . fmap) taskPoints a
    let wg = (fmap . fmap) weight a

    taskTileZones task
    es <- simpleList cs (crumbTask task)
    tab <- tabsTask

    _ <- widgetHold (tableTurnpoints task legs) $
            (\case
                TaskTabValidity -> viewValidity vy vw
                TaskTabScore -> tableScore utc ln vy wg ps tp s
                TaskTabTask -> tableTurnpoints task legs
                TaskTabMap -> do
                    task' <- sample . current $ task
                    route' <- sample . current $ route
                    map task' route'
                TaskTabAbsent -> tableAbsent t task)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ = return never
