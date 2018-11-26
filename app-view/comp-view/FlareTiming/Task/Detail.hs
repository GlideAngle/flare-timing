module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp(..), Task(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Track.Point
    (Validity(..), Allocation(..), Breakdown(..))
import FlareTiming.Comp (compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Quality.Validity (tableValidity)
import FlareTiming.Task.Quality.Allocation (tableAllocation)
import FlareTiming.Task.Score (tableScore)
import FlareTiming.Task.Quality.Weight (tableWeight)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)

taskDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> Dynamic t [(Pilot, Breakdown)]
    -> m (Event t IxTask)

taskDetail cs x v a s = do
    _ <- simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tab <- tabsTask
    let utc = utcOffset . head <$> cs

    _ <- widgetHold (tableScore utc s) $
            (\case
                TaskTabScore -> tableScore utc s

                TaskTabQuality -> do
                    let ps = (fmap . fmap) points a
                    let tp = (fmap . fmap) taskPoints a

                    tableValidity v
                    tableAllocation ps tp
                    tableWeight $ (fmap . fmap) weight a

                TaskTabTask -> tableTurnpoints x
                TaskTabMap -> do y <- sample . current $ x; map y
                TaskTabAbsent -> tableAbsent x)
            <$> tab

    return $ switchDyn (leftmost <$> es)
