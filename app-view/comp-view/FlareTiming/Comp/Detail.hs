module FlareTiming.Comp.Detail (compDetail) where

import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp, Task, Pilot)
import FlareTiming.Comp (comps)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comp.Tab (CompTab(..), tabsComp)
import FlareTiming.Task.List (taskList)
import FlareTiming.Comp.Pilot (tablePilot)

compDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Pilot]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
compDetail cs ps xs = do
    comps cs
    tab <- tabsComp

    e <- widgetHold (taskList xs) $
            (\case
                CompTabTask -> taskList xs
                CompTabPilot -> do tablePilot ps; return never)
            <$> tab

    return $ switchDyn e
