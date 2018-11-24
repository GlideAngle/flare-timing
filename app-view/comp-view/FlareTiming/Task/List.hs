module FlareTiming.Task.List (taskList) where

import Reflex
import Reflex.Dom

import WireTypes.Comp (Task(..))
import FlareTiming.Events (IxTask(..))
import FlareTiming.Task.ListItem (liTask)

listToIxTask :: Reflex t => [Event t ()] -> Event t IxTask
listToIxTask =
    leftmost
    . zipWith (\i x -> (const $ IxTask i) <$> x) [1..]

taskList
    :: MonadWidget t m
    => Dynamic t [Task]
    -> m (Event t IxTask)
taskList xs = do
    ys <- el "ul" $ simpleList xs liTask
    return $ switchDyn (listToIxTask <$> ys)
