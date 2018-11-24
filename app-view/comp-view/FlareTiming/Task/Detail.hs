module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp(..), Task(..))
import WireTypes.Track.Point (Validity(..), Allocation(..))
import FlareTiming.Comp (compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Quality.Validity (tableValidity)
import FlareTiming.Task.Quality.Allocation (tableAllocation)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)

taskDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail cs x v a = do
    _ <- simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tab <- tabsTask

    _ <- widgetHold (text "score") $
            (\case
                TaskTabScore -> text "score"

                TaskTabQuality -> do
                    tableValidity v
                    tableAllocation a

                TaskTabTask -> tableTurnpoints x
                TaskTabMap -> do y <- sample . current $ x; map y
                TaskTabAbsent -> tableAbsent x)
            <$> tab

    return $ switchDyn (leftmost <$> es)
