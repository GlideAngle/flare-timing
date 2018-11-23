module FlareTiming.View (view) where

import FlareTiming.NavBar (navbar)
import FlareTiming.Comp (getComps)
import FlareTiming.Footer (footer)
import FlareTiming.Task (tasks)

import Reflex.Dom (MonadWidget)

view :: MonadWidget t m => m ()
view = do
    navbar
    tasks
    footer
    return ()
