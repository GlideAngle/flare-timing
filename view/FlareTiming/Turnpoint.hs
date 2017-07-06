{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module FlareTiming.Turnpoint
    ( turnpoint
    , turnpointRadius
    , getName
    , getNameRadius
    ) where

import Reflex.Dom (MonadWidget , Dynamic , el , dynText)
import qualified Data.Text as T (Text, pack)

import Data.Flight.Types (Turnpoint(..), showRadius)

getNameRadius :: Turnpoint -> String
getNameRadius (Turnpoint name _ _ radius) = name ++ " " ++ showRadius radius

getName :: Turnpoint -> String
getName (Turnpoint name _ _ _) = name

turnpointRadius :: forall t (m :: * -> *). MonadWidget t m => Dynamic t Turnpoint -> m ()
turnpointRadius x = do
    dynText $ fmap (T.pack . getNameRadius) x

turnpoint :: forall t (m :: * -> *). MonadWidget t m => Dynamic t Turnpoint -> m ()
turnpoint x = do
    dynText $ fmap (T.pack . getName) x
