{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.XmlPickle
    ( xpNewtypeRational
    , xpNewtypeQuantity
    , xpNewtypeLat
    , xpNewtypeLng
    , xpBool
    ) where

import Control.Newtype (Newtype(..))
import Data.UnitsOfMeasure (unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle (PU(..), xpWrap, xpPrim)

xpNewtypeLat :: Newtype n Rational => PU n
xpNewtypeLat =
    xpWrap
        ( pack . (toRational :: Double -> _)
        , fromRational . unpack
        )
        xpPrim

xpNewtypeLng :: Newtype n Rational => PU n
xpNewtypeLng =
    xpWrap
        ( pack . (toRational :: Double -> _)
        , fromRational . unpack
        )
        xpPrim

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
