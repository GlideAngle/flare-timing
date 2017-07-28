{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-|
Module      : Flight.Task
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides tasks for hang gliding and paragliding competitons. These are routes to fly,
from launch to goal passing through each control zone of the course in order along the way.
-}
module Flight.Task
    ( -- * Control Zones
      Lat(..)
    , Lng(..)
    , LatLng(..)
    , Radius(..)
    , center
    , radius
    , separatedZones
    , Incline(..)
    , Bearing(..)
    , Zone(..)
    , Deadline(..)
    , TimeOfDay(..)
    , Interval(..)
    , StartGates(..)
    , Task(..)
    , TaskDistance(..)
    , Epsilon(..)
    , earthRadius
    , defEps
    -- * Spherical Distance Between Points
    , distancePointToPoint
    , distanceHaversine
    , distanceHaversineF
    -- * Optimized Shortest Path 
    , Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , EdgeDistance(..)
    , DistancePath(..)
    , circumSample
    , distanceEdgeToEdge
    -- * Angle unit conversion
    , degToRad
    , degToRadLL
    , radToDeg
    , radToDegLL
    ) where

import Flight.Zone
import Flight.Geo
import Flight.PointToPoint
import Flight.EdgeToEdge
import Flight.Separated
import Flight.CylinderEdge
