{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Flight.PointToPoint.Segment (SpanLatLng) where

import Data.UnitsOfMeasure (u)

import Flight.LatLng (LatLng(..))
import Flight.Distance (TaskDistance(..))

-- | A function for measuring the distance between two points given as
-- latitude longitude pairs in radians.
type SpanLatLng a
    = LatLng a [u| rad |]
    -> LatLng a [u| rad |]
    -> TaskDistance a
