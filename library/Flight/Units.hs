{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Units
    ( Length
    , abs
    ) where

import Data.Ratio ((%))
import Data.UnitsOfMeasure (u, declareConvertibleUnit)
import Data.UnitsOfMeasure.Internal (Quantity(..))

type Length u = Quantity Rational u

[u| s, m |]

[u| min = 60 s, h = 3600 s, d = 86400 s |]

[u| ft = 100 % 328 m, mi = 1609.344 m, mph = mi/h |]

[u| rad |]
[u| deg = (5030569068109113 % 288230376151711744) rad |]
