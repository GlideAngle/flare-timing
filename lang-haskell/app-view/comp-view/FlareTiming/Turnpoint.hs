module FlareTiming.Turnpoint
    ( turnpoint
    , turnpointRadius
    , getName
    , getLat
    , getLng
    , getRadius
    , getGiveIn
    , getGiveOut
    , getAlt
    ) where

import Reflex.Dom (MonadWidget, Dynamic, dynText)
import qualified Data.Text as T (Text, pack)

import WireTypes.Zone (RawZone(..))
import WireTypes.ZoneKind (showRadius, showLat, showLng, showAlt)

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

getGiveIn :: RawZone -> T.Text
getGiveIn RawZone{giveIn} = maybe "" (T.pack . showRadius) giveIn

getGiveOut :: RawZone -> T.Text
getGiveOut RawZone{giveOut} = maybe "" (T.pack . showRadius) giveOut

getAlt :: RawZone -> T.Text
getAlt RawZone{alt} = maybe "" (T.pack . showAlt) alt

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
