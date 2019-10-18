{-# LANGUAGE DuplicateRecordFields #-}

module Flight.Geodesy.Problem
    ( GeodesyProblems(..)
    , DirectProblem(..)
    , DirectSolution(..)
    , InverseProblem(..)
    , InverseSolution(..)
    , DProb, DSoln
    , IProb, ISoln
    ) where

import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.Units.DegMinSec (DMS(..))
import Flight.Distance (QTaskDistance)

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
data InverseSolution s α =
    InverseSolution
        { s :: s -- ^ The distance between departure and arrival points.
        , α₁ :: α -- ^ The azimuth at the departure point.
        , α₂ :: Maybe α -- ^ The azimuth at the arrival point.
        }

type DProb = DirectProblem (DMS, DMS) DMS (QTaskDistance Double [u| m |])
type DSoln = DirectSolution (DMS, DMS) DMS

type IProb = InverseProblem (DMS, DMS)
type ISoln = InverseSolution (QTaskDistance Double [u| m |]) DMS

class GeodesyProblems a α s where
    direct
        :: InverseProblem a
        -> InverseSolution s α
        -> Maybe (DirectProblem a α s, DirectSolution a α)

    inverse
        :: DirectProblem a α s
        -> DirectSolution a α
        -> Maybe (InverseProblem a, InverseSolution s α)

instance Angle α => GeodesyProblems a α s where
    direct _ InverseSolution{α₂ = Nothing} = Nothing
    direct InverseProblem{x = y, y = x} InverseSolution{s, α₁, α₂ = Just α₂} =
        Just
            ( DirectProblem{x, α₁ = flip180 α₂, s}
            , DirectSolution{y, α₂ = Just $ flip180 α₁}
            )

    inverse _ DirectSolution{α₂ = Nothing} = Nothing
    inverse DirectProblem{x = y, α₁, s} DirectSolution{y = x, α₂ = Just α₂} =
        Just
            ( InverseProblem{x, y}
            , InverseSolution{s, α₁ = flip180 α₂, α₂ = Just $ flip180 α₁}
            )

flip180 :: Angle a => a -> a
flip180 x = rotate x $ fromQuantity [u| 180 deg |]
