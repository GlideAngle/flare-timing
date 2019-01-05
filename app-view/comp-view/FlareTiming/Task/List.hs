module FlareTiming.Task.List (taskList) where

import Reflex
import Reflex.Dom

import WireTypes.Comp (Task(..))
import WireTypes.Route (TaskDistance)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Task.ListItem (liTask)

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [TaskDistance]
    -> Dynamic t [Task]
    -> m (Event t IxTask)
taskList lens xs = do
    let ixs = zip (IxTask <$> [1..]) <$> xs
    ys <- el "ul" $ simpleList ixs (liTask lens)
    return $ switchDyn (listToIxTask <$> ys)
