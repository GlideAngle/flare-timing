module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp(..), Task(..))
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms (getTaskScore, getTaskValidityWorking)
import FlareTiming.Comp (compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Score (tableScore)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)
import FlareTiming.Task.Validity (viewValidity)

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail t@(IxTask _) cs x vy a = do
    s <- getTaskScore t
    vw <- getTaskValidityWorking t
    _ <- simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tab <- tabsTask
    let utc = utcOffset . head <$> cs
    let ps = (fmap . fmap) points a
    let tp = (fmap . fmap) taskPoints a
    let wg = (fmap . fmap) weight a

    _ <- widgetHold (tableScore utc vy wg ps tp s) $
            (\case
                TaskTabValidity -> viewValidity vy vw
                TaskTabScore -> tableScore utc vy wg ps tp s
                TaskTabTask -> tableTurnpoints x
                TaskTabMap -> do y <- sample . current $ x; map y
                TaskTabAbsent -> tableAbsent x)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ = return never
