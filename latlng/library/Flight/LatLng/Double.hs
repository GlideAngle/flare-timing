{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}

module Flight.LatLng.Double (showAngle, degToRad, radToDeg) where

import Data.Text.Lazy (unpack)
import Formatting ((%), format)
import qualified Formatting.ShortFormatters as Fmt (f)
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Show (showUnit, showQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)
import Flight.Units ()
import Flight.Units.DegMinSec (fromQ)

showAngle
    :: (KnownUnit (Unpack u), Convertible u [u| deg |])
    => Quantity Double u -> String
showAngle q@(MkQuantity x) =
    case showUnit q of
        "rad" -> unpack $ format (Fmt.f 8 % "rad") x
        "deg" -> unpack $ format (Fmt.f 8 % "°") x
        "dms" -> show . fromQ $ q
        _ -> showQuantity q

-- | Conversion of degrees to radians.
degToRad :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
degToRad (MkQuantity x) =
    MkQuantity $ x * pi / 180

-- | Conversion of radians to degrees.
radToDeg :: Quantity Double [u| rad |] -> Quantity Double [u| deg |]
radToDeg (MkQuantity x) =
    MkQuantity $ x * 180 / pi
