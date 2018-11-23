module FlareTiming.Turnpoint
    ( turnpoint
    , turnpointRadius
    , getName
    , getLat
    , getLng
    , getRadius
    ) where

import Reflex.Dom (MonadWidget, Dynamic, dynText)
import qualified Data.Text as T (Text, pack)

import Data.Flight.Types (RawZone(..), showRadius, showLat, showLng)

getNameRadius :: RawZone -> T.Text
getNameRadius RawZone{zoneName,radius} =
    T.pack $ zoneName ++ " " ++ showRadius radius

getName :: RawZone -> T.Text
getName RawZone{zoneName} = T.pack zoneName

getLat :: RawZone -> T.Text
getLat RawZone{lat} = T.pack . showLat $ lat

getLng :: RawZone -> T.Text
getLng RawZone{lng} = T.pack . showLng $ lng

getRadius :: RawZone -> T.Text
getRadius RawZone{radius} = T.pack . showRadius $ radius

turnpointRadius
    :: MonadWidget t m
    => Dynamic t RawZone
    -> m ()
turnpointRadius x = do
    dynText $ fmap getNameRadius x

turnpoint
    :: MonadWidget t m
    => Dynamic t RawZone
    -> m ()
turnpoint x = do
    dynText $ fmap getName x
