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
    ( -- * Spherical Distance Between Points
      TaskDistance(..)
    , distancePointToPoint
    , distanceHaversine
    , distanceHaversineF
    , costSegment
    , fromKms
    -- * Optimized Shortest Path 
    , Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , PathDistance(..)
    , circumSample
    , distanceEdgeToEdge
    , costEastNorth
    -- * Zones
    , separatedZones
    ) where

import Flight.Distance
import Flight.PointToPoint
import Flight.EdgeToEdge
import Flight.Projected
import Flight.Separated
import Flight.CylinderEdge
