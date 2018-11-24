module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp(..), Task(..))
import WireTypes.Track.Point (Validity)
import FlareTiming.Comp (compTask)
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map (map)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Validity (tableValidity)
import FlareTiming.Task.Turnpoints (tableTurnpoints)
import FlareTiming.Task.Absent (tableAbsent)

taskDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> m (Event t IxTask)

taskDetail cs x v = do
    _ <- simpleList cs (compTask x)
    es <- simpleList cs (crumbTask x)
    tab <- tabsTask

    _ <- widgetHold (text "score") $
            (\case
                TaskTabScore -> text "score"
                TaskTabValidity -> tableValidity v
                TaskTabTask -> tableTurnpoints x
                TaskTabMap -> do y <- sample . current $ x; map y
                TaskTabAbsent -> tableAbsent x)
            <$> tab

    return $ switchDyn (leftmost <$> es)
