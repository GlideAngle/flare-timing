{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-|
Module      : Flight.Task
Copyright   : (c) Block Scope Limited 2017
License     : BSD3
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides tasks for hang gliding and paragliding competitons. These are routes
to fly, from launch to goal passing through each control zone of the course in
order along the way.
-}
module Flight.Task
    ( -- * Optimized Shortest Path 
      SpanLatLng
    , Samples(..)
    , Tolerance(..)
    , SampleParams(..)
    , CircumSample
    , CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , ToTrackLine(..)
    , Zs(..)
    , distanceEdgeToEdge
    -- * Zones
    , separatedZones
    , fromZs
    ) where

import Flight.PointToPoint.Segment (SpanLatLng)
import Flight.EdgeToEdge
import Flight.Separated
import Flight.Cylinder.Sample
import Flight.Cylinder.Edge
import Flight.ShortestPath
import Flight.TaskTrack.Internal (ToTrackLine(..))
