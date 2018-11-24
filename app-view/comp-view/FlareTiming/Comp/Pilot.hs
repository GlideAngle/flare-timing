module FlareTiming.Comp.Pilot (tablePilot) where

import Reflex.Dom

import WireTypes.Comp (Pilot)
import FlareTiming.Pilot.Row (row)

tablePilot
    :: MonadWidget t m
    => Dynamic t [Pilot]
    -> m ()
tablePilot xs = do
    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Id"
                    el "th" $ text "Name"

                simpleList xs row
    return ()
