module FlareTiming.View (view) where

import FlareTiming.Task (tasks)
import FlareTiming.Comp (comps)

import Reflex.Dom (MonadWidget)

view :: MonadWidget t m => m ()
view = do
    comps
    tasks
