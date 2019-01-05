module FlareTiming.Comp.Detail (compDetail) where

import Reflex
import Reflex.Dom

import WireTypes.Comp (Comp, Task)
import WireTypes.Route (TaskDistance)
import FlareTiming.Comp (comps)
import FlareTiming.Breadcrumb (crumbComp)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comp.Tab (CompTab(..), tabsComp)
import FlareTiming.Task.List (taskList)
import FlareTiming.Comp.Pilot (tablePilot)
import FlareTiming.Comp.Settings (tableComp)

compDetail
    :: MonadWidget t m
    => Dynamic t [TaskDistance]
    -> Dynamic t [Comp]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
compDetail ls cs ts = do
    comps cs
    _ <- simpleList cs crumbComp
    tab <- tabsComp

    e <- widgetHold (taskList ls ts) $
            (\case
                CompTabSettings -> do
                    _ <- simpleList cs tableComp
                    return never

                CompTabTask -> taskList ls ts

                CompTabPilot -> do
                    tablePilot ts
                    return never)
            <$> tab

    return $ switchDyn e
