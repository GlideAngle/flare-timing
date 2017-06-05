{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-|
Module      : Flight.Task
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides tasks for hang gliding and paragliding competitons. These are routes to fly
from launch to goal through course line way points.
-}
module Flight.Task
    ( -- * Control zones and distance between them. 
      ShowAngle(..)
    , LatLng(..)
    , Radius(..)
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
    , TaskDistance(..)
    , earthRadius
    , distanceEdgeToEdge
    , distancePointToPoint
    , distanceHaversine
    , distanceHaversineF
    , separatedZones
    , center
    , radius
    -- * Zone edge-to-edge shortest path.
    , Samples(..)
    , Tolerance(..)
    , circumSample
    , distanceEdgeToEdge
    -- * Zone separation.
    , separatedZones
    ) where

import Flight.Zone
import Flight.Geo
import Flight.PointToPoint
import Flight.EdgeToEdge
import Flight.Separated
