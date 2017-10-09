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
    , fromKms
    -- * Optimized Shortest Path 
    , Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , EdgeDistance(..)
    , DistancePath(..)
    , circumSample
    , distanceEdgeToEdge
    -- * Zones
    , separatedZones
    ) where

import Flight.PointToPoint
import Flight.ShortestPath
import Flight.EdgeToEdge
import Flight.Separated
import Flight.CylinderEdge
