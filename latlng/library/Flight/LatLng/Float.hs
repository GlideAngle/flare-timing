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

module Flight.LatLng.Float (showAngle, degToRad, radToDeg) where

import Data.Text.Lazy (unpack)
import Formatting ((%), format)
import qualified Formatting.ShortFormatters as Fmt (f)
import Data.UnitsOfMeasure (KnownUnit, Unpack, u)
import Data.UnitsOfMeasure.Show (showUnit, showQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Flight.Units ()

showAngle :: KnownUnit (Unpack u) => Quantity Float u -> String
showAngle q@(MkQuantity x) =
    case showUnit q of
        "rad" -> unpack $ format (Fmt.f 8 % "rad") x
        "deg" -> unpack $ format (Fmt.f 8 % "°") x
        _ -> showQuantity q

-- | Conversion of degrees to radians.
degToRad :: Quantity Float [u| deg |] -> Quantity Float [u| rad |]
degToRad (MkQuantity x) =
    MkQuantity $ x * pi / 180

-- | Conversion of radians to degrees.
radToDeg :: Quantity Float [u| rad |] -> Quantity Float [u| deg |]
radToDeg (MkQuantity x) =
    MkQuantity $ x * 180 / pi
