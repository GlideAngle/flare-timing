{-# LANGUAGE MonoLocalBinds #-}

module FlareTiming.View (view) where

import FlareTiming.NavBar (navbar)
import FlareTiming.Footer (footer)
import FlareTiming.Breadcrumb (breadcrumb)
import FlareTiming.Task (tasks)
import FlareTiming.Comp (comps)

import Reflex.Dom (MonadWidget)

view :: MonadWidget t m => m ()
view = do
    navbar
    comps
    breadcrumb
    tasks
    footer
    return ()
