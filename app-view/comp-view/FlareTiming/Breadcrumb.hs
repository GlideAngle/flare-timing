{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module FlareTiming.Breadcrumb (breadcrumb) where

import Reflex.Dom
    ( MonadWidget
    , elClass, el, text
    )

breadcrumb :: MonadWidget t m => m ()
breadcrumb =
    elClass "nav" "breadcrumb" $ do
        el "ul" $ do
            el "li" $
                el "a" $ text "Comp"
