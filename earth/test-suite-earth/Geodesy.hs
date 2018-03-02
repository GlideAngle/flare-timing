{-# LANGUAGE DuplicateRecordFields #-}

module Geodesy
    ( DirectProblem(..)
    , DirectSolution(..)
    , InverseProblem(..)
    , InverseSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    ) where

import Flight.Units ()
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (TaskDistance(..))

-- | The inputs for the direct or forward problem in geodesy.
data DirectProblem a α s =
    DirectProblem
        { x :: a -- ^ The departure point on the ellipsoid.
        , α₁ :: α -- ^ The azimuth from the departure point.
        , s :: s -- ^ The distance to the arrival point.
        }

-- | The inputs for the inverse or reverse problem in geodesy.
data InverseProblem a =
    InverseProblem
        { x :: a -- ^ The departure point.
        , y :: a -- ^ The arrival point.
        }

-- | The outputs for the solution to the direct or forward problem in geodesy.
data DirectSolution a α =
    DirectSolution
        { y :: a -- ^ The arrival point.
        , α₂ :: Maybe α -- ^ The azimuth at the arrival point.
        }

-- | The outputs for the solution to the inverse or reverse problem in geodesy.
data InverseSolution a α =
    InverseSolution
        { s :: a -- ^ The distance between departure and arrival points.
        , α₁ :: α -- ^ The azimuth at the departure point.
        , α₂ :: Maybe α -- ^ The azimuth at the arrival point.
        }

type DProb = DirectProblem (DMS, DMS) DMS (TaskDistance Double)
type DSoln = DirectSolution (DMS, DMS) DMS

type IProb = InverseProblem (DMS, DMS)
type ISoln = InverseSolution (TaskDistance Double) DMS
