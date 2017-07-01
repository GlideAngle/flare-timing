{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Turnpoint (liTurnpointRadius) where

import Reflex.Dom (MonadWidget , Dynamic , el , dynText)
import qualified Data.Text as T (Text, pack)

import FlareTiming.WireTypes (Turnpoint(..), showRadius)

liTurnpointRadius :: forall t (m :: * -> *).
             MonadWidget t m =>
             Dynamic t Turnpoint -> m ()
liTurnpointRadius x = do
    let dyTp :: Dynamic t T.Text =
            fmap (\(Turnpoint name _ _ radius) ->
                T.pack $ name ++ " " ++ showRadius radius) x

    el "li" $ do
        dynText dyTp
