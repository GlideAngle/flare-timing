{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.XmlPickle (xpNewtypeRational, xpNewtypeQuantity) where

import Control.Newtype (Newtype(..))
import Data.UnitsOfMeasure (unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle (PU(..), xpWrap, xpPrim)

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
