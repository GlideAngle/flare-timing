{-|
Module      : Flight.Task
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Provides tasks for hang gliding and paragliding competitons. These are routes
to fly, from launch to goal passing through each control zone of the course in
order along the way.
-}
module Flight.Task
    ( -- * Optimized Shortest Path 
      CostSegment
    , DistancePointToPoint
    , AngleCut(..)
    , Zs(..)
    -- * Zones
    , fromZs
    ) where

import Flight.ShortestPath.Cost
