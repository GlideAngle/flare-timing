{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.XmlPickle
    ( xpNewtypeRational
    , xpNewtypeQuantity
    , xpNewtypeLat
    , xpNewtypeLng
    , xpBool
    , xpUtcOffset
    ) where

import Text.Printf (printf)
import Control.Newtype (Newtype(..))
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle (XmlPickler(..), PU(..), xpWrap, xpPrim, xpText)

import Flight.Fsdb.Internal.Parse (parseDegree, parseUtcOffset)
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Comp (UtcOffset(..))

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

-- | A pickler for UtcOffset.
--
-- >>> pickleDoc xpUtcOffset (UtcOffset 0)
-- NTree (XTag "/" []) [NTree (XText "0") []]
--
-- >>> pickleDoc xpUtcOffset (UtcOffset 60)
-- NTree (XTag "/" []) [NTree (XText "+1") []]
--
-- >>> pickleDoc xpUtcOffset (UtcOffset 90)
-- NTree (XTag "/" []) [NTree (XText "+1.50") []]
--
-- >>> pickleDoc xpUtcOffset (UtcOffset (negate 60))
-- NTree (XTag "/" []) [NTree (XText "-1") []]
--
-- >>> pickleDoc xpUtcOffset (UtcOffset (negate 90))
-- NTree (XTag "/" []) [NTree (XText "-1.50") []]
--
-- >>> unpickleDoc' xpUtcOffset $ pickleDoc xpUtcOffset (UtcOffset 0)
-- Right (UtcOffset {timeZoneMinutes = 0})
--
-- >>> unpickleDoc' xpUtcOffset $ pickleDoc xpUtcOffset (UtcOffset 60)
-- Right (UtcOffset {timeZoneMinutes = 60})
xpUtcOffset :: PU UtcOffset
xpUtcOffset =
    xpWrap
        ( \utcHrs ->
            fromMaybe (error $ printf "Can't parse %s as UTC offset." utcHrs)
            $ parseUtcOffset utcHrs
        , \(UtcOffset utcMins) ->
            case utcMins `compare` 0 of
                EQ -> "0"
                GT ->
                    case utcMins `divMod` 60 of
                        (x, 0) -> printf "+%d" x
                        _ -> printf "+%.2f" $ (fromIntegral utcMins :: Double) / 60.0
                LT ->
                    case utcMins `divMod` 60 of
                        (x, 0) -> printf "%d" x
                        _ -> printf "%.2f" $ (fromIntegral utcMins :: Double) / 60.0
        )
        xpText

-- $setup
-- >>> import Text.XML.HXT.Arrow.Pickle.Xml
-- >>> import Flight.LatLng.Raw (RawLat(..), RawLng(..))
-- >>> import Text.Xml.Pickle (fromXML)
