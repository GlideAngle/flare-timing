{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.XmlPickle
    ( xpNewtypeRational
    , xpNewtypeQuantity
    , xpNewtypeLat
    , xpNewtypeLng
    , xpBool
    ) where

import Control.Newtype (Newtype(..))
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle (XmlPickler(..), PU(..), xpWrap, xpPrim, xpText)

import Flight.Fsdb.Internal.Parse (parseDegree)
import Flight.LatLng.Raw (RawLat(..), RawLng(..))

-- |
-- >>> fromXML "33.21354" :: Maybe RawLat
-- Just 33.21354
--
-- >>> fromXML "-33.21354" :: Maybe RawLat
-- Just -33.21354
--
-- >>> fromXML "33 12.8124" :: Maybe RawLat
-- Just 33.21354
--
-- >>> fromXML "-33 12.8124" :: Maybe RawLat
-- Just -33.21354
--
-- >>> fromXML "33 12 48.744" :: Maybe RawLat
-- Just 33.21354
--
-- >>> fromXML "-33 12 48.744" :: Maybe RawLat
-- Just -33.21354
instance XmlPickler RawLat where
    xpickle = xpNewtypeLat

instance XmlPickler RawLng where
    xpickle = xpNewtypeLng

xpDegree :: PU Double
xpDegree =
    xpWrap
        ( fromMaybe 0 . parseDegree
        , show
        )
        xpText

-- |
-- >>> pickleDoc xpNewtypeLat (RawLat 33.21354)
-- NTree (XTag "/" []) [NTree (XText "33.21354") []]
--
-- >>> unpickleDoc xpNewtypeLat $ pickleDoc xpNewtypeLat (RawLat 33.21354) :: Maybe RawLat
-- Just 33.21354
xpNewtypeLat :: Newtype n Rational => PU n
xpNewtypeLat =
    xpWrap
        ( pack . (toRational :: Double -> _)
        , fromRational . unpack
        )
        xpDegree

xpNewtypeLng :: Newtype n Rational => PU n
xpNewtypeLng =
    xpWrap
        ( pack . (toRational :: Double -> _)
        , fromRational . unpack
        )
        xpDegree

xpNewtypeRational :: Newtype n Rational => PU n
xpNewtypeRational =
    xpWrap
        ( pack . (toRational :: Double -> _)
        , fromRational . unpack
        )
        xpPrim

xpNewtypeQuantity :: Newtype n (Quantity Double u) => PU n
xpNewtypeQuantity =
    xpWrap
        ( pack . MkQuantity
        , unQuantity . unpack
        )
        xpPrim

-- | A pickler for Bool when 0 is False and 1 is True.
--
-- >>> pickleDoc xpBool False
-- NTree (XTag "/" []) [NTree (XText "0") []]
--
-- >>> pickleDoc xpBool True
-- NTree (XTag "/" []) [NTree (XText "1") []]
--
-- >>> unpickleDoc' xpBool $ pickleDoc xpBool False
-- Right False
--
-- >>> unpickleDoc' xpBool $ pickleDoc xpBool True
-- Right True
xpBool :: PU Bool
xpBool =
    xpWrap
        ( toEnum
        , fromEnum
        )
        xpPrim

-- $setup
-- >>> import Text.XML.HXT.Arrow.Pickle.Xml
-- >>> import Flight.LatLng.Raw (RawLat(..), RawLng(..))
-- >>> import Text.Xml.Pickle (fromXML)
