module FlareTiming.View (view) where

import FlareTiming.Task (tasks)

import Reflex.Dom (MonadWidget)

view :: MonadWidget t m => m ()
view = tasks
