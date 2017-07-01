{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Turnpoint (turnpointRadius, turnpoint) where

import Reflex.Dom (MonadWidget , Dynamic , el , dynText)
import qualified Data.Text as T (Text, pack)

import FlareTiming.WireTypes (Turnpoint(..), showRadius)

getNameRadius :: Turnpoint -> T.Text
getNameRadius (Turnpoint name _ _ radius) = T.pack $ name ++ " " ++ showRadius radius

getName :: Turnpoint -> T.Text
getName (Turnpoint name _ _ _) = T.pack name

turnpointRadius :: forall t (m :: * -> *). MonadWidget t m => Dynamic t Turnpoint -> m ()
turnpointRadius x = do
    dynText $ fmap getNameRadius x

turnpoint :: forall t (m :: * -> *). MonadWidget t m => Dynamic t Turnpoint -> m ()
turnpoint x = do
    dynText $ fmap getName x
