module FlareTiming.View (view) where

import FlareTiming.Footer (footer)
import FlareTiming.Task (tasks)

import Reflex.Dom (MonadWidget)

view :: MonadWidget t m => m ()
view = do
    tasks
    footer
    return ()
