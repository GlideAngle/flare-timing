module FlareTiming.Comp.Pilot (tablePilot) where

import Reflex.Dom

import WireTypes.Pilot (Pilot)
import FlareTiming.Pilot (rowPilot)

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

                simpleList xs rowPilot
    return ()
