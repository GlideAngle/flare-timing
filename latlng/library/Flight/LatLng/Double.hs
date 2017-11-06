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
import Flight.Units ()

showAngle :: KnownUnit (Unpack u) => Quantity Double u -> String
showAngle q@(MkQuantity x) =
    case showUnit q of
        "deg" -> unpack $ format (Fmt.f 2 % "°") x
        "rad" -> unpack $ format (Fmt.f 2 % "rad") x
        _ -> showQuantity q

-- | Conversion of degrees to radians.
degToRad :: Quantity Double [u| deg |] -> Quantity Double [u| rad |]
degToRad (MkQuantity x) =
    MkQuantity $ x * pi / 180

-- | Conversion of radians to degrees.
radToDeg :: Quantity Double [u| rad |] -> Quantity Double [u| deg |]
radToDeg (MkQuantity x) =
    MkQuantity $ x * 180 / pi
