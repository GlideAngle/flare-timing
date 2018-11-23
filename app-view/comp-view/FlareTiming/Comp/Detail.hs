module FlareTiming.Comp.Detail (compDetail) where

import Reflex
import Reflex.Dom

import Data.Flight.Types (Comp(..), Task(..))
import FlareTiming.Comp (comps)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Comp.Tab (CompTab(..), tabsComp)
import FlareTiming.Task.List (taskList)

compDetail
    :: MonadWidget t m
    => Dynamic t [Comp]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
compDetail cs xs = do
    comps cs
    tab <- tabsComp

    e <- widgetHold (taskList xs) $
            (\case
                CompTabTask -> taskList xs
                CompTabPilot -> do
                    text "pilots"
                    return never)
            <$> tab

    return $ switchDyn e
