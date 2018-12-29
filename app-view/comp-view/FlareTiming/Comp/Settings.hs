module FlareTiming.Comp.Settings (tableComp) where

import Reflex.Dom

import WireTypes.Comp (Comp)

tableComp
    :: MonadWidget t m
    => Dynamic t Comp
    -> m ()
tableComp _ = do
    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Setting"
                    el "th" $ text "Value"

                    return ()
    return ()
